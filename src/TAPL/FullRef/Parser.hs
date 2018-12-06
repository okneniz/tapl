{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullRef.Parser where

import TAPL.FullRef.Types
import TAPL.FullRef.Context
import TAPL.FullRef.Lexer

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.Either (isLeft, isRight)

import Prelude hiding (succ, pred, lookup)
import qualified Prelude (lookup)

import Data.List (findIndex)

type LCParser = Parsec String (FullRefContext Term) Term
type LCTypeParser = Parsec String (FullRefContext Term) Type

parse :: String -> String -> Either ParseError (FullRefContext AST)
parse code path = runParser fullRefParser pureContext path code
          where pureContext = FullRefContext withoutNames emptyMemory (TUnit Nothing)
                withoutNames = []
                emptyMemory = LCMemory []
                unit = TUnit Nothing

parseFile :: String -> IO (Either ParseError (FullRefContext AST))
parseFile path = do
    code <- readFile path
    return $ parse code path

fullRefParser :: Parsec String (FullRefContext Term) (FullRefContext AST)
fullRefParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of FullRefContext names memory _ -> FullRefContext names memory ast)

term :: LCParser
term = try (abstraction <?> "abstraction")
   <|> try apply
   <|> try notApply
   <|> parens term

padded :: Stream s m Char => String -> ParsecT s u m String
padded x = spaces *> string x <* spaces

lookup' :: LCParser -> LCParser -> LCParser
lookup' key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'

dotRef :: LCParser -> Term -> LCParser
dotRef key t = do
    _ <- char '.'
    pos <- getPosition
    i <- key
    t' <- (try $ dotRef key (TLookup (Just pos) t i)) <|> (return $ TLookup (Just pos) t i)
    return t'

anotated :: LCParser -> LCParser
anotated e = do
    t <- e
    t' <- (try $ ascribed t) <|> (return t)
    return t'

ascribed :: Term -> LCParser
ascribed t = do
    spaces
    _ <- string "as"
    optional spaces
    ty <- typeAnnotation
    pos <- getPosition
    return $ TAscribe (Just pos) t ty

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp (Just pos)

notApply :: LCParser
notApply = try value
       <|> try ((variant value) <?> "variant")
       <|> try (assign <?> "assignment")
       <|> try (condition <?> "condition")
       <|> try (let' <?> "let")
       <|> try (deref <?> "deref")
       <|> try (fix <?> "fix")
       <|> try (case' <?> "case")
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)

assign :: LCParser
assign = chainl1 notAssign $ do
           p <- getPosition
           reservedOp ":="
           return $ TAssign (Just p)

notAssign :: LCParser
notAssign = try value
        <|> try (condition <?> "condition")
        <|> try (deref <?> "deref")
        <|> try (ref <?> "ref")
        <|> try (fix <?> "fix")
        <|> try (variable <?> "variable")
        <|> try (parens notAssign)

value :: LCParser
value = anotated $ try (boolean <?> "boolean")
               <|> try (string' <?> "string")
               <|> try (succ <?> "succ")
               <|> try (pred <?> "pred")
               <|> try (isZero <?> "isZero?")
               <|> try (zero <?> "zero")
               <|> try (float <?> "float")
               <|> try (integer <?> "integer")
               <|> try (unit <?> "unit")
               <|> try (record <?> "record")
               <|> try (pair <?> "pair")

ref :: LCParser
ref = fun "ref" TRef

deref :: LCParser
deref = do
    reservedOp "!"
    p <- getPosition
    t <- term
    return $ TDeref (Just p) t

fix :: LCParser
fix = do
    reserved "fix"
    p <- getPosition
    t <- term
    return $ TFix (Just p) t

isZero :: LCParser
isZero = fun "zero?" TIsZero

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

boolean :: LCParser
boolean = try true <|> try false
    where true = constant "true" TTrue
          false = constant "false" TFalse

string' :: LCParser
string' = do
    p <- getPosition
    t <- try stringLiteral
    return $ TString (Just p) t

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

float :: LCParser
float = do
    p <- getPosition
    n <- floatNum
    return $ TFloat (Just p) n

integer :: LCParser
integer = do
    p <- getPosition
    n <- natural
    return $ TInt (Just p) n

constant :: String -> (Info -> Term) -> LCParser
constant name t = do
    p <- getPosition
    reserved name
    return $ t (Just p)

fun :: String -> (Info -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- term
    return $ tm (Just p) t

condition :: LCParser
condition = do
    p <- getPosition
    reserved "if"
    t1 <- term
    reserved "then"
    t2 <- term
    reserved "else"
    t3 <- term
    return $ TIf (Just p) t1 t2 t3

let' :: LCParser
let' = do
    reserved "let"
    p <- getPosition
    v <- identifier
    reserved "="
    t1 <- term
    optional spaces
    reserved "in"
    optional spaces
    context <- getState
    modifyState $ \c -> addName c v
    t2 <- term
    return $ TLet (Just p) v t1 t2

case' :: LCParser
case' = do
  reserved "case"
  t <- term
  optional spaces
  reserved "of"
  optional spaces
  branches <- branch `sepBy` (reservedOp "|")
  pos <- getPosition
  return $ TCase (Just pos) t branches
  where branch = do
          (caseName, varName) <- pattern
          reservedOp "->"
          context <- getState
          modifyState $ \c -> addName c varName
          t2 <- term
          setState context
          return (caseName, varName, t2)
        pattern = angles $ do
          caseName <- identifier
          reservedOp "="
          varName <- identifier
          return (caseName, varName)

pair :: LCParser
pair = lookup' integer $ braces $ do
    t1 <- term
    comma
    t2 <- term
    pos <- getPosition
    return $ TPair (Just pos) t1 t2

record :: LCParser
record = lookup' keyword $ braces $ do
    ts <- (keyValue "=") `sepBy` comma
    p <- getPosition
    return $ TRecord (Just p) ts

keyword :: LCParser
keyword = do
  word <- identifier
  p <- getPosition
  return $ TKeyword (Just p) word

keyValue :: String -> Parsec String (FullRefContext Term) (String, Term)
keyValue s = do
    k <- identifier
    optional spaces
    string s
    optional spaces
    v <- term
    optional spaces
    return (k, v)

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    dot
    optional spaces
    context <- getState
    setState $ bind context varName $ (VarBind varType)
    t <- term
    setState context
    return $ TAbs (Just p) varName varType t

variable :: LCParser
variable = anotated $ lookup' (try integer <|> try keyword) $ do
    name <- identifier
    context <- getState
    let ns = names context
    p <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar (Just p) n (length $ ns)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

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
  return $ TTag (Just pos) key t ty

-- Types annotations --

termType :: LCTypeParser
termType = do
    colon
    ty <- typeAnnotation
    return ty

typeAnnotation :: LCTypeParser
typeAnnotation = (try arrowAnnotation <?> "arrow type annotation")
             <|> try notArrowAnnotation
             <|> (try $ parens typeAnnotation)

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
                    optional spaces
                    string "->"
                    optional spaces
                    return $ TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = try (booleanAnnotation <?> "boolean type")
                 <|> try (stringAnnotation  <?> "string type")
                 <|> try (natAnnotation     <?> "nat type")
                 <|> try (floatAnnotation   <?> "float type")
                 <|> try (intAnnotation     <?> "int type")
                 <|> try (unitAnnotation    <?> "unit type")
                 <|> try (refAnnotation     <?> "ref type")
                 <|> try (topAnnotation     <?> "top type")
                 <|> try (botAnnotation     <?> "bot type")
                 <|> try (idTypeAnnotation  <?> "atomic type")
                 <|> try (productAnnotation <?> "product type")
                 <|> try (recordAnnotation  <?> "record annotation")
                 <|> try (variantAnnotation <?> "variant annotation")

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    string name
    return ty

idTypeAnnotation :: LCTypeParser
idTypeAnnotation = do
    i <- oneOf ['A'..'Z']
    d <- many $ oneOf ['a'..'z']
    return $ TyID (i:d)

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
    optional spaces
    string "*"
    optional spaces
    ty2 <- typeAnnotation
    return $ TyProduct ty1 ty2

recordAnnotation :: LCTypeParser
recordAnnotation = braces $ do
    tys <- keyValue2 `sepBy` comma
    return $ TyRecord tys

keyValue2 :: Parsec String (FullRefContext Term) (String, Type)
keyValue2 = do
    k <- identifier
    colon
    v <- typeAnnotation
    return (k, v)

keyType :: Char -> Parsec String (FullRefContext Term) (String, Type)
keyType c = do
     k <- identifier
     char c
     v <- typeAnnotation
     return (k, v)

variantAnnotation :: LCTypeParser
variantAnnotation = do
    string "<"
    ts <- (keyType ':') `sepBy` (padded ",")
    string ">"
    return $ TyVariant ts
