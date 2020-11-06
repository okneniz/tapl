module Language.TAPL.FullEquirec.Parser (parse) where

import Language.TAPL.FullEquirec.Types
import Language.TAPL.FullEquirec.Context
import Language.TAPL.FullEquirec.Lexer
import Language.TAPL.Common.Helpers (ucid, padded)
import Language.TAPL.Common.Context (findVarName)

import Prelude hiding (abs, succ, pred)
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser fullEquirecParser []

fullEquirecParser :: Parsec String LCNames ([Command], LCNames)
fullEquirecParser =  (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = padded $ bindCommand <|> evalCommand

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces
    reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = apply <|> notApply <|> parens term

apply :: LCParser
apply = try $ chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = value
       <|> (isZero <?> "zero?")
       <|> (timesFloat <?> "timesfloat")
       <|> (variant <?> "variant")
       <|> (condition <?> "condition")
       <|> (letT <?> "let")
       <|> (fix <?> "fix")
       <|> (caseT <?> "case")
       <|> (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> try (parens notApply)

value :: LCParser
value = optionalAscribed $ (boolean <?> "boolean")
                       <|> (stringT <?> "string")
                       <|> (float <?> "float")
                       <|> (nat <?> "nat")
                       <|> (unit <?> "unit")
                       <|> ((optionalProjection identifier record) <?> "record")
                       <|> ((optionalProjection pairIndexes pair) <?> "pair")

abstraction :: LCParser
abstraction = try $ optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- colon *> typeAnnotation
    names <- getState
    modifyState $ addVar name ty
    t <- dot *> term
    setState names
    return $ TAbs pos name ty t

variable :: LCParser
variable = optionalAscribed $ optionalProjection (pairIndexes <|> identifier) $ do
    name <- identifier
    n <- getState
    pos <- getPosition
    case findVarName n name of
         Just x -> return $ TVar pos x (length n)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

stringT :: LCParser
stringT = TString <$> getPosition <*> try stringLiteral

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> term

nat :: LCParser
nat = succ <|> pred <|> zero <|> integer
    where succ = fun "succ" TSucc
          pred = fun "pred" TPred
          zero = constant "zero" TZero
          integer = do
            p <- getPosition
            i <- try natural
            toNat p i (TZero p)
          toNat _ i _ | i < 0 = unexpected $ "unexpected negative number"
          toNat _ 0 t = return t
          toNat p i t = toNat p (i - 1) (TSucc p t)

isZero :: LCParser
isZero = fun "zero?" TIsZero

float :: LCParser
float = TFloat <$> getPosition <*> try floatNum

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name *> (t <$> getPosition)

unit :: LCParser
unit = constant "unit" TUnit

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
    where dotRef k t = do
              pos <- dot *> getPosition
              i <- k
              t' <- (try $ dotRef key (TProj pos t i)) <|> (return $ TProj pos t i)
              return t'

optionalAscribed :: LCParser -> LCParser
optionalAscribed e = do
    t <- e
    t' <- (try $ f t) <|> (return t)
    return t'
  where f t = do
          spaces
          reserved "as"
          optional spaces
          ty <- typeAnnotation
          pos <- getPosition
          return $ TAscribe pos t ty

pair :: LCParser
pair = try $ braces $ TPair <$> getPosition <*> (term <* comma) <*> term

pairIndexes :: Parsec String LCNames String
pairIndexes = flip(:) [] <$> oneOf "01"

record :: LCParser
record = try $ braces $ do
    ts <- (keyValue (reservedOp "=") term) `sepBy` comma
    pos <- getPosition
    return $ TRecord pos $ Map.fromList ts

letT :: LCParser
letT = do
    p <- reserved "let" *> getPosition
    v <- identifier
    t1 <- reservedOp "=" *> padded term <* reserved "in"
    context <- getState
    modifyState $ addName v
    t2 <- term
    setState context
    return $ TLet  p v t1 t2

caseT :: LCParser
caseT = TCase <$> getPosition
              <*> (reserved "case" *> padded term <* reserved "of")
              <*> (Map.fromList <$> branch `sepBy` (reserved "|"))
  where branch = do
          (caseName, varName) <- angles $ keyValue (reservedOp "=") identifier
          reservedOp "->"
          context <- getState
          modifyState $ addName varName
          t2 <- term
          setState context
          return (caseName, (varName, t2))

variant :: LCParser
variant = try $ do
  pos <- getPosition
  (key, t) <- angles $ keyValue (reservedOp "=") notApply
  ty <- reserved "as" *> typeAnnotation
  return $ TTag pos key t ty

fix :: LCParser
fix = TFix <$> (reserved "fix" *> getPosition) <*> term

timesFloat :: LCParser
timesFloat = TTimesFloat <$> (reserved "timesfloat" *> getPosition) <*> notApply <*> (spaces *> notApply)

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = try $ chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
  padded (reservedOp "->") *> return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation
                 <|> stringAnnotation
                 <|> unitAnnotation
                 <|> natAnnotation
                 <|> floatAnnotation
                 <|> topAnnotation
                 <|> botAnnotation
                 <|> recursiveType
                 <|> variantAnnotation
                 <|> productAnnotation
                 <|> recordAnnotation
                 <|> typeVarOrID

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

stringAnnotation :: LCTypeParser
stringAnnotation = primitiveType "String" TyString

unitAnnotation :: LCTypeParser
unitAnnotation = primitiveType "Unit" TyUnit

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

floatAnnotation :: LCTypeParser
floatAnnotation = primitiveType "Float" TyFloat

productAnnotation :: LCTypeParser
productAnnotation = try $ braces $ chainl1 typeAnnotation $ padded (reservedOp "*") *> return TyProduct

recordAnnotation :: LCTypeParser
recordAnnotation = try $ braces $ TyRecord <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

variantAnnotation :: LCTypeParser
variantAnnotation = angles $ TyVariant <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name *> return ty

recursiveType :: LCTypeParser
recursiveType = do
    x <- reserved "Rec" *> ucid <* dot
    names <- getState
    modifyState $ addName x
    ty <- typeAnnotation
    setState names
    return $ TyRec x ty

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    n <- getState
    return $ case findVarName n name of
                  Just varName -> TyVar varName (length n)
                  Nothing -> TyID name

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> f
