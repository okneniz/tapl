module Language.TAPL.FullRef.Parser (parse) where

import Language.TAPL.FullRef.Types
import Language.TAPL.FullRef.Context
import Language.TAPL.FullRef.Lexer
import Language.TAPL.Common.Helpers (ucid, padded)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.Functor (($>))
import Data.List (findIndex)
import qualified Data.Map.Lazy as Map

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser fullRefParser []

fullRefParser :: Parsec String LCNames ([Command], LCNames)
fullRefParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = padded (bindCommand <|> evalCommand)

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
apply = try $ chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = value
       <|> (isZero <?> "isZero?")
       <|> (variant <?> "variant")
       <|> (assignT <?> "assignment")
       <|> (condition <?> "condition")
       <|> (letT <?> "let")
       <|> (derefT <?> "deref")
       <|> (fix <?> "fix")
       <|> (caseT <?> "case")
       <|> (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> try (parens notApply)

assignT :: LCParser
assignT = try $ chainl1 notAssign $ reservedOp ":=" *> (TAssign <$> getPosition)

notAssign :: LCParser
notAssign = value
        <|> (isZero <?> "isZero?")
        <|> (condition <?> "condition")
        <|> (derefT <?> "deref")
        <|> (ref <?> "ref")
        <|> (fix <?> "fix")
        <|> (variable <?> "variable")
        <|> (parens notAssign)

value :: LCParser
value = optionalAscribed $ (boolean <?> "boolean")
                       <|> (stringT <?> "string")
                       <|> (float <?> "float")
                       <|> (nat <?> "nat")
                       <|> (unit <?> "unit")
                       <|> ((optionalProjection identifier record) <?> "record")
                       <|> ((optionalProjection pairIndexes pair) <?> "pair")

ref :: LCParser
ref = fun "ref" TRef

derefT :: LCParser
derefT = TDeref <$> (reservedOp "!" *> getPosition) <*> term

fix :: LCParser
fix = TFix <$> (reserved "fix" *> getPosition) <*> term

isZero :: LCParser
isZero = fun "zero?" TIsZero

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

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

stringT :: LCParser
stringT = TString <$> getPosition <*> try stringLiteral

unit :: LCParser
unit = constant "unit" TUnit

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

letT :: LCParser
letT = do
    p <- getPosition <* reserved "let"
    name <- identifier <* reservedOp "="
    t1 <- term <* reserved "in"
    n <- getState
    modifyState $ addName name
    t2 <- term
    setState n
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

pair :: LCParser
pair = try $ braces $ TPair <$> getPosition <*> (term <* comma) <*> term

pairIndexes :: Parsec String LCNames String
pairIndexes = flip(:) [] <$> oneOf "01"

record :: LCParser
record = try $ braces $ do
    ts <- (keyValue (reservedOp "=") term) `sepBy` comma
    p <- getPosition
    return $ TRecord p $ Map.fromList ts

variant :: LCParser
variant = try $ do
  pos <- getPosition
  (key, t) <- angles $ keyValue (reservedOp "=") notApply
  ty <- reserved "as" *> typeAnnotation
  return $ TTag pos key t ty

abstraction :: LCParser
abstraction = optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    ty <- colon *> typeAnnotation
    n <- getState
    modifyState $ addVar name ty
    t <- dot *> term
    setState n
    return $ TAbs pos name ty t

variable :: LCParser
variable = optionalAscribed $ optionalProjection (pairIndexes <|> identifier) $ do
    name <- identifier
    p <- getPosition
    ns <- getState
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar p n (length $ ns)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context " <> " " <> (show p)

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    (try $ dotRef key t) <|> (return t)
    where dotRef k t = do
            pos <- dot *> getPosition
            i <- k
            (try $ dotRef key (TProj pos t i)) <|> (return $ TProj pos t i)

optionalAscribed :: LCParser -> LCParser
optionalAscribed e = do
    t <- e
    (try $ f t) <|> (return t)
  where f t = do
          padded $ reserved "as"
          ty <- typeAnnotation
          pos <- getPosition
          return $ TAscribe pos t ty

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") $> TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = (booleanAnnotation     <?> "boolean type")
                 <|> (stringAnnotation      <?> "string type")
                 <|> (natAnnotation         <?> "nat type")
                 <|> (floatAnnotation       <?> "float type")
                 <|> (intAnnotation         <?> "int type")
                 <|> (unitAnnotation        <?> "unit type")
                 <|> (refAnnotation         <?> "ref type")
                 <|> (topAnnotation         <?> "top type")
                 <|> (botAnnotation         <?> "bot type")
                 <|> (idTypeAnnotation      <?> "atomic type")
                 <|> try (productAnnotation <?> "product type")
                 <|> try (recordAnnotation  <?> "record annotation")
                 <|> try (variantAnnotation <?> "variant annotation")

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name $> ty

idTypeAnnotation :: LCTypeParser
idTypeAnnotation = TyID <$> ucid

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

stringAnnotation :: LCTypeParser
stringAnnotation = primitiveType "String" TyString

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

floatAnnotation :: LCTypeParser
floatAnnotation = primitiveType "Float" TyFloat

intAnnotation :: LCTypeParser
intAnnotation = primitiveType "Int" TyFloat

unitAnnotation :: LCTypeParser
unitAnnotation = primitiveType "Unit" TyUnit

refAnnotation :: LCTypeParser
refAnnotation = TyRef <$> (reserved "Ref" *> typeAnnotation)

productAnnotation :: LCTypeParser
productAnnotation = try $ braces $ chainl1 typeAnnotation $ reservedOp "*" $> TyProduct

recordAnnotation :: LCTypeParser
recordAnnotation = try $ braces $ TyRecord <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

variantAnnotation :: LCTypeParser
variantAnnotation = angles $ TyVariant <$> Map.fromList <$> (keyValue colon typeAnnotation) `sepBy` comma

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> f
