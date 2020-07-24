module Language.TAPL.FullEquirec.Parser (parse) where

import Language.TAPL.FullEquirec.Types
import Language.TAPL.FullEquirec.Context
import Language.TAPL.FullEquirec.Lexer
import Language.TAPL.Common.Helpers (ucid)

import Prelude hiding (abs, succ, pred)
import qualified Data.Map.Lazy as Map

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser reconParser []

reconParser :: Parsec String LCNames ([Command], LCNames)
reconParser = do
    commands <- command `sepEndBy` semi
    eof
    names <- getState
    return $ (commands, names)

command :: Parsec String LCNames Command
command =  (try bindCommand) <|> (try evalCommand)

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces
    reserved "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = try $ Eval <$> term `sepEndBy` semi

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp pos

notApply :: LCParser
notApply = try value
       <|> try (timesFloat <?> "timesfloat")
       <|> try ((variant value) <?> "variant")
       <|> try (condition <?> "condition")
       <|> try (let' <?> "let")
       <|> try (fix <?> "fix")
       <|> try (case' <?> "case")
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)
       <|> try (parens apply)

notTypeBind :: LCParser
notTypeBind = try apply
          <|> try notApply
          <|> try (parens notTypeBind)

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
    pos <- getPosition
    reserved "lambda"
    name <- identifier
    ty <- termType
    dot
    optional spaces
    names <- getState
    modifyState $ addVar name ty
    t <- notTypeBind
    setState names
    return $ TAbs pos name ty t

variable :: LCParser
variable = optionalAscribed $ projection (integer <|> keyword) $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar pos n (length names)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

string' :: LCParser
string' = TString <$> getPosition <*> try stringLiteral

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

nat :: LCParser
nat = zero <|> succ <|> pred

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> term

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
constant name t = reserved name >> (t <$> getPosition)

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> notTypeBind)
                <*> (reserved "then" *> notTypeBind)
                <*> (reserved "else" *> notTypeBind)

projection :: LCParser -> LCParser -> LCParser
projection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
  where dotRef key t = do
          dot
          pos <- getPosition
          i <- key
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
pair = projection integer $ braces $ do
    t1 <- notTypeBind
    comma
    t2 <- notTypeBind
    pos <- getPosition
    return $ TPair pos t1 t2

record :: LCParser
record = projection keyword $ braces $ do
    ts <- (keyValue (reservedOp "=") term) `sepBy` comma
    pos <- getPosition
    return $ TRecord pos $ Map.fromList ts

keyword :: LCParser
keyword = do
  word <- identifier
  p <- getPosition
  return $ TKeyword p word

let' :: LCParser
let' = do
    reserved "let"
    p <- getPosition
    name <- identifier
    reservedOp "="
    t1 <- notTypeBind
    optional spaces
    reserved "in"
    optional spaces
    names <- getState
    modifyState $ addName name
    t2 <- notTypeBind
    return $ TLet p name t1 t2

case' :: LCParser
case' = do
  reserved "case"
  t <- notTypeBind
  optional spaces
  reserved "of"
  optional spaces
  branches <- branch `sepBy` (reservedOp "|")
  pos <- getPosition
  return $ TCase pos t $ Map.fromList branches
  where branch = do
          (caseName, varName) <- pattern
          reservedOp "->"
          names <- getState
          modifyState $ addName varName
          t2 <- notTypeBind
          setState names
          return (caseName, (varName, t2))
        pattern = angles $ do
          caseName <- identifier
          reservedOp "="
          name <- identifier
          return (caseName, name)


variant :: LCParser -> LCParser
variant x = do
  reservedOp "<"
  pos <- getPosition
  key <- identifier
  reservedOp "="
  t <- x
  reservedOp ">"
  reserved "as"
  ty <- typeAnnotation
  return $ TTag pos key t ty

fix :: LCParser
fix = do
    reserved "fix"
    t <- notTypeBind
    pos <- getPosition
    return $ TFix pos t

timesFloat :: LCParser
timesFloat = do
    reserved "timesfloat"
    pos <- getPosition
    t1 <- notApply
    spaces
    t2 <- notApply
    return $ TTimesFloat pos t1 t2

keyValue devider val = do
  key <- identifier
  devider
  value <- val
  return (key,value)

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try recursiveType
             <|> try arrowAnnotation
             <|> notArrowAnnotation

recursiveType :: LCTypeParser
recursiveType = do
    reserved "Rec"
    x <- spaces *> ucid <* dot
    names <- getState
    modifyState $ addName x
    ty <- typeAnnotation
    setState names
    return $ TyRec x ty

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    optional spaces
    reservedOp "->"
    optional spaces
    return $ TyArrow

productAnnotation :: LCTypeParser
productAnnotation = braces $ chainl1 typeAnnotation $ do
    reservedOp "*"
    return $ TyProduct

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation
                 <|> try productAnnotation
                 <|> try recordAnnotation
                 <|> try variantAnnotation
                 <|> try stringAnnotation
                 <|> try unitAnnotation
                 <|> try natAnnotation
                 <|> try floatAnnotation
                 <|> try topAnnotation
                 <|> try botAnnotation
                 <|> try typeVarOrID

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

typeVarOrID:: LCTypeParser
typeVarOrID = do
    name <- ucid
    names <- getState
    return $ case findVarName names name of
                  Just varName -> TyVar varName (length names)
                  Nothing -> TyID name
