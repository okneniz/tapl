module Language.TAPL.FullSimple.Parser (parse) where

import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Context
import Language.TAPL.FullSimple.Lexer
import Language.TAPL.Common.Helpers (ucid)

import Prelude hiding (abs, succ, pred)
import qualified Data.Map.Lazy as Map
import Data.List (findIndex)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser reconParser []

simpleParser :: Parsec String LCNames ([Command], LCNames)
simpleParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command =  (try bindCommand) <|> (try evalCommand)

bindCommand :: LCCommandParser
bindCommand = do
    p <- getPosition
    x <- ucid
    reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind p x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = try $ Eval <$> term `sepEndBy` semi

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ TApp <$> getPosition

notApply :: LCParser
notApply = value
       <|> ((variant value) <?> "variant")
       <|> (condition <?> "condition")
       <|> (let' <?> "let")
       <|> (fix <?> "fix")
       <|> (case' <?> "case")
       <|> (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> (parens notApply)

value :: LCParser
value = optionalAscribed $ (boolean <?> "boolean")
                       <|> (string' <?> "string")
                       <|> (succ <?> "succ")
                       <|> (pred <?> "pred")
                       <|> (isZero <?> "zero?")
                       <|> (zero <?> "zero")
                       <|> (float <?> "float")
                       <|> (integer <?> "integer")
                       <|> (unit <?> "unit")
                       <|> try (record <?> "record")
                       <|> try (pair <?> "pair")

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    _ <- dot
    optional spaces
    context <- getState
    modifyState $ bind varName (VarBind varType)
    t <- term
    setState context
    return $ TAbs p varName varType t

variable :: LCParser
variable = optionalAscribed $ projection (integer <|> keyword) $ do
    name <- identifier
    names <- getState
    p <- getPosition
    case findIndex ((== name) . fst) names of
         Just n -> return $ TVar p n (length $ names)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

string' :: LCParser
string' = TString <$> getPosition <*> try stringLiteral

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = reserved name *> (tm <$> getPosition <*> term)

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

float :: LCParser
float = TFloat <$> getPosition <*> floatNum

integer :: LCParser
integer = TInt <$> getPosition <*> natural

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name *> (t <$> getPosition)

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

projection :: LCParser -> LCParser -> LCParser
projection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
  where dotRef k t = do
          _ <- dot
          p <- getPosition
          i <- k
          t' <- (try $ dotRef key (TProj p t i)) <|> (return $ TProj p t i)
          return t'

optionalAscribed :: LCParser -> LCParser
optionalAscribed e = do
    t <- e
    t' <- (try $ f t) <|> (return t)
    return t'
  where f t = TAscribe <$> getPosition
                       <*> pure t
                       <*> (reserved "as" *> typeAnnotation)

pair :: LCParser
pair = projection integer $ braces $ TPair <$> getPosition
                                           <*> (term <* comma)
                                           <*> term

record :: LCParser
record = projection keyword $ braces $ do
    ts <- keyValue (reservedOp "=") term `sepBy` comma
    p <- getPosition
    return $ TRecord p $ Map.fromList ts

keyword :: LCParser
keyword = TKeyword <$> getPosition <*> identifier

let' :: LCParser
let' = do
    reserved "let"
    p <- getPosition
    (v, t1) <- keyValue (reservedOp "=") term
    reserved "in"
    context <- getState
    modifyState $ addName v
    t2 <- term
    setState context
    return $ TLet p v t1 t2

case' :: LCParser
case' = do
  reserved "case"
  t <- term
  optional spaces *> reserved "of" <* optional spaces
  branches <- branch `sepBy` (reservedOp "|")
  p <- getPosition
  return $ TCase p t $ Map.fromList branches
  where branch = do
          (caseName, varName) <- pattern
          reservedOp "->"
          context <- getState
          modifyState $ addName varName
          t2 <- term
          setState context
          return (caseName, (varName, t2))
        pattern = angles $ keyValue (reservedOp "=") identifier

variant :: LCParser -> LCParser
variant x = do
  p <- getPosition
  (k, t) <- angles $ keyValue (reservedOp "=") x
  ty <- reserved "as" *> variantAnnotation
  return $ TTag p k t ty

fix :: LCParser
fix = TFix <$> (reserved "fix" *> getPosition) <*> term

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation
             <|> try productAnnotation
             <|> try recordAnnotation
             <|> try variantAnnotation
             <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    padded (reservedOp "->")
    return $ TyArrow

productAnnotation :: LCTypeParser
productAnnotation = braces $ chainl1 typeAnnotation $ do
    padded (reservedOp "*")
    return TyProduct

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation
                 <|> stringAnnotation
                 <|> unitAnnotation
                 <|> natAnnotation
                 <|> floatAnnotation
                 <|> baseTypeAnnotation
                 <|> topAnnotation
                 <|> botAnnotation

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

baseTypeAnnotation :: LCTypeParser
baseTypeAnnotation = TyID <$> ucid

recordAnnotation :: LCTypeParser
recordAnnotation = braces $ do
    tys <- (keyValue colon typeAnnotation) `sepBy` comma
    return $ TyRecord $ Map.fromList tys

variantAnnotation :: LCTypeParser
variantAnnotation = angles $ do
    ts <- (keyValue colon typeAnnotation) `sepBy` comma
    return $ TyVariant $ Map.fromList ts

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

padded :: Parsec String LCNames a -> Parsec String LCNames a
padded x = optional spaces *> x <* optional spaces

