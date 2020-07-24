module Language.TAPL.FullRef.Parser (parse) where

import Language.TAPL.FullRef.Types
import Language.TAPL.FullRef.Context
import Language.TAPL.FullRef.Lexer
import Language.TAPL.Common.Helpers (ucid)

import Prelude hiding (abs, succ, pred)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.List (findIndex)
import qualified Data.Map.Lazy as Map

type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError (AST, LCNames)
parse = runParser fullRefParser []

fullRefParser :: Parsec String LCNames (AST, LCNames)
fullRefParser = do
    ast <- term `sepEndBy` semi
    eof
    names <- getState
    return (ast, names)

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

projection :: LCParser -> LCParser -> LCParser
projection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'

dotRef :: LCParser -> Term -> LCParser
dotRef key t = do
    _ <- dot
    pos <- getPosition
    i <- key
    t' <- (try $ dotRef key (TProj pos t i)) <|> (return $ TProj pos t i)
    return t'

anotated :: LCParser -> LCParser
anotated e = do
    t <- e
    t' <- (try $ ascribed t) <|> (return t)
    return t'

ascribed :: Term -> LCParser
ascribed t = do
    spaces
    reserved "as"
    optional spaces
    ty <- typeAnnotation
    pos <- getPosition
    return $ TAscribe pos t ty

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp pos

notApply :: LCParser
notApply = value
       <|> ((variant value) <?> "variant")
       <|> try (assign <?> "assignment")
       <|> (condition <?> "condition")
       <|> (let' <?> "let")
       <|> (deref <?> "deref")
       <|> (fix <?> "fix")
       <|> (case' <?> "case")
       <|> (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> (parens notApply)

assign :: LCParser
assign = chainl1 notAssign $ do
           p <- getPosition
           reservedOp ":="
           return $ TAssign p

notAssign :: LCParser
notAssign = value
        <|> (condition <?> "condition")
        <|> (deref <?> "deref")
        <|> (ref <?> "ref")
        <|> (fix <?> "fix")
        <|> (variable <?> "variable")
        <|> (parens notAssign)

value :: LCParser
value = anotated $ (boolean <?> "boolean")
               <|> (string' <?> "string")
               <|> (succ <?> "succ")
               <|> (pred <?> "pred")
               <|> (isZero <?> "isZero?")
               <|> (zero <?> "zero")
               <|> (float <?> "float")
               <|> (integer <?> "integer")
               <|> (unit <?> "unit")
               <|> try (record <?> "record")
               <|> try (pair <?> "pair")

ref :: LCParser
ref = fun "ref" TRef

deref :: LCParser
deref = do
    reservedOp "!"
    p <- getPosition
    t <- term
    return $ TDeref p t

fix :: LCParser
fix = do
    reserved "fix"
    p <- getPosition
    t <- term
    return $ TFix p t

isZero :: LCParser
isZero = fun "zero?" TIsZero

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

string' :: LCParser
string' = TString <$> getPosition <*> try stringLiteral

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

float :: LCParser
float = TFloat <$> getPosition <*> floatNum

integer :: LCParser
integer = TInt <$> getPosition <*> natural

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name >> (t <$> getPosition)

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name *> getPosition) <*> term

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

let' :: LCParser
let' = do
    reserved "let"
    p <- getPosition
    v <- identifier
    reservedOp "="
    t1 <- term
    optional spaces
    reserved "in"
    optional spaces
    context <- getState
    modifyState $ \c -> addName c v
    t2 <- term
    setState context
    return $ TLet p v t1 t2

case' :: LCParser
case' = do
  reserved "case"
  t <- term
  optional spaces
  reserved "of"
  optional spaces
  branches <- branch `sepBy` (reservedOp "|")
  pos <- getPosition
  return $ TCase pos t $ Map.fromList branches
  where branch = do
          (caseName, varName) <- pattern
          reservedOp "->"
          context <- getState
          modifyState $ \c -> addName c varName
          t2 <- term
          setState context
          return (caseName, (varName, t2))
        pattern = angles $ do
          caseName <- identifier
          reservedOp "="
          varName <- identifier
          return (caseName, varName)

pair :: LCParser
pair = projection integer $ braces $ do
    t1 <- term
    _ <- comma
    t2 <- term
    pos <- getPosition
    return $ TPair pos t1 t2

record :: LCParser
record = projection keyword $ braces $ do
    ts <- (keyValue (reservedOp "=") term) `sepBy` comma
    p <- getPosition
    return $ TRecord p $ Map.fromList ts

keyword :: LCParser
keyword = do
  word <- identifier
  p <- getPosition
  return $ TKeyword p word

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
variable = anotated $ projection (try integer <|> try keyword) $ do
    name <- identifier
    p <- getPosition
    ns <- getState
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar p n (length $ ns)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

variant :: LCParser -> LCParser
variant x = do
  reservedOp "<"
  pos <- getPosition
  key <- identifier
  reservedOp "="
  t <- x
  reservedOp ">"
  reserved "as"
  ty <- variantAnnotation
  return $ TTag pos key t ty

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = do
  k <- identifier
  _ <- devider
  v <- val
  return (k,v)

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = (try arrowAnnotation <?> "arrow type annotation")
             <|> try notArrowAnnotation
             <|> (try $ parens typeAnnotation)

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
                    optional spaces
                    reservedOp "->"
                    optional spaces
                    return $ TyArrow

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
primitiveType name ty = reserved name >> return ty

idTypeAnnotation :: LCTypeParser
idTypeAnnotation = do
    x <- ucid
    return $ TyID x

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
refAnnotation = do
    reserved "Ref"
    ty <- typeAnnotation
    return $ TyRef ty

productAnnotation :: LCTypeParser
productAnnotation = braces $ do
    ty1 <- typeAnnotation
    reservedOp "*"
    ty2 <- typeAnnotation
    return $ TyProduct ty1 ty2

recordAnnotation :: LCTypeParser
recordAnnotation = braces $ do
    tys <- (keyValue colon typeAnnotation) `sepBy` comma
    return $ TyRecord $ Map.fromList tys

variantAnnotation :: LCTypeParser
variantAnnotation = angles $ do
    ts <- (keyValue colon typeAnnotation) `sepBy` comma
    return $ TyVariant $ Map.fromList ts
