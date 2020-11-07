module Language.TAPL.FullSimple.Parser (parse) where

import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Context
import Language.TAPL.FullSimple.Lexer
import Language.TAPL.Common.Helpers (ucid, padded)

import qualified Data.Map.Lazy as Map
import Data.List (findIndex)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser simpleParser []

simpleParser :: Parsec String LCNames ([Command], LCNames)
simpleParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = padded $ bindCommand <|> evalCommand

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces <* reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = apply <|> notApply <|> parens term

apply :: LCParser
apply = chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = value
       <|> (timesFloat <?> "timesfloat")
       <|> (isZero <?> "zero?")
       <|> (condition <?> "condition")
       <|> (letT <?> "let")
       <|> (fix <?> "fix")
       <|> (caseT <?> "case")
       <|> (variable <?> "variable")
       <|> try (parens notApply)

value :: LCParser
value = optionalAscribed $ (boolean <?> "boolean")
                       <|> (unit <?> "unit")
                       <|> (stringT <?> "string")
                       <|> (float <?> "float")
                       <|> (nat <?> "nat")
                       <|> (variant <?> "variant")
                       <|> (abstraction <?> "abstraction")
                       <|> ((optionalProjection identifier record) <?> "record")
                       <|> ((optionalProjection pairIndexes pair) <?> "pair")


isZero :: LCParser
isZero = fun "zero?" TIsZero

abstraction :: LCParser
abstraction = optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- colon >> typeAnnotation
    names <- getState
    modifyState $ addVar name ty
    t <- dot *> term
    setState names
    return $ TAbs pos name ty t

variable :: LCParser
variable = optionalAscribed $ optionalProjection (pairIndexes <|> identifier) $ do
    name <- identifier
    names <- getState
    p <- getPosition
    case findIndex ((== name) . fst) names of
         Just n -> return $ TVar p n (length names)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

unit :: LCParser
unit = constant "unit" TUnit

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

stringT :: LCParser
stringT = TString <$> getPosition <*> try stringLiteral

float :: LCParser
float = TFloat <$> getPosition <*> try floatNum

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name *> (t <$> getPosition)

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> term

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

record :: LCParser
record = try $ braces $ TRecord <$> getPosition
                                <*> (Map.fromList <$> (keyValue (reservedOp "=") term) `sepBy` comma)

pair :: LCParser
pair = try $ braces $ TPair <$> getPosition <*> (term <* comma) <*> term

pairIndexes :: Parsec String LCNames String
pairIndexes = flip(:) [] <$> oneOf "01"

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    (try $ dotRef key t) <|> (return t)
    where dotRef k t1 = do
            pos <- dot *> getPosition
            i <- k
            (try $ dotRef key (TProj pos t1 i)) <|> (return $ TProj pos t1 i)

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

letT :: LCParser
letT = do
    p <- getPosition <* reserved "let"
    name <- identifier <* reservedOp "="
    t1 <- term <* reserved "in"
    names <- getState
    modifyState $ addName name
    t2 <- term
    setState names
    return $ TLet p name t1 t2

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

timesFloat :: LCParser
timesFloat = TTimesFloat <$> (reserved "timesfloat" *> getPosition) <*> notApply <*> (spaces *> notApply)

variant :: LCParser
variant = try $ do
  pos <- getPosition
  (key, t) <- angles $ keyValue (reservedOp "=") term
  ty <- reserved "as" *> variantAnnotation
  return $ TTag pos key t ty

fix :: LCParser
fix = TFix <$> (reserved "fix" *> getPosition) <*> term

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation)
                $ padded (reservedOp "->") *> return TyArrow

productAnnotation :: LCTypeParser
productAnnotation = braces $ chainl1 typeAnnotation $ padded (reservedOp "*") *> return TyProduct

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation
                 <|> floatAnnotation
                 <|> stringAnnotation
                 <|> unitAnnotation
                 <|> natAnnotation
                 <|> recordAnnotation
                 <|> productAnnotation
                 <|> variantAnnotation
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

recordAnnotation :: LCTypeParser
recordAnnotation = braces $ TyRecord <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

variantAnnotation :: LCTypeParser
variantAnnotation = angles $ TyVariant <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name *> return ty

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    n <- getState
    return $ case findIndex ((== name) . fst) n of
                  Just x -> TyVar x (length n)
                  Nothing -> TyID name

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> f
