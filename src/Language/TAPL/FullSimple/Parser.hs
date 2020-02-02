{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullSimple.Parser (parse) where

import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Context
import Language.TAPL.FullSimple.Lexer

import Prelude hiding (abs, succ, pred)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.List (findIndex)
import qualified Data.Map.Strict as Map

type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError (AST, LCNames)
parse = runParser fullSimpleParser []

fullSimpleParser :: Parsec String LCNames (AST, LCNames)
fullSimpleParser = do
    ast <- term `sepEndBy` semi
    eof
    names <- getState
    return (ast, names)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp (infoFrom pos)

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
    pos <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    _ <- dot
    optional spaces
    context <- getState
    modifyState $ bind varName (VarBind varType)
    t <- term
    setState context
    return $ TAbs (infoFrom pos) varName varType t

variable :: LCParser
variable = optionalAscribed $ lookup' (integer <|> keyword) $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) names of
         Just n -> return $ TVar (infoFrom pos) n (length $ names)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

string' :: LCParser
string' = do
    p <- getPosition
    t <- try stringLiteral
    return $ TString (infoFrom p) t

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (Info -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- term
    return $ tm (infoFrom p) t

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

float :: LCParser
float = do
    pos <- getPosition
    n <- floatNum
    return $ TFloat (infoFrom pos) n

integer :: LCParser
integer = do
    pos <- getPosition
    n <- natural
    return $ TInt (infoFrom pos) n

constant :: String -> (Info -> Term) -> LCParser
constant name t = do
    p <- getPosition
    reserved name
    return $ t (infoFrom p)

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

condition :: LCParser
condition = do
    reserved "if"
    x <- term
    reserved "then"
    y <- term
    reserved "else"
    z <- term
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

lookup' :: LCParser -> LCParser -> LCParser
lookup' key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
  where dotRef k t = do
          _ <- dot
          pos <- getPosition
          i <- k
          t' <- (try $ dotRef key (TLookup (infoFrom pos) t i)) <|> (return $ TLookup (infoFrom pos) t i)
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
          return $ TAscribe (infoFrom pos) t ty

pair :: LCParser
pair = lookup' integer $ braces $ do
    t1 <- term
    _ <- comma
    t2 <- term
    pos <- getPosition
    return $ TPair (infoFrom pos) t1 t2

record :: LCParser
record = lookup' keyword $ braces $ do
    ts <- (keyValue (reservedOp "=") term) `sepBy` comma
    pos <- getPosition
    return $ TRecord (infoFrom pos) $ Map.fromList ts

keyword :: LCParser
keyword = do
  word <- identifier
  p <- getPosition
  return $ TKeyword (infoFrom p) word

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
    return $ TLet (infoFrom p) v t1 t2

case' :: LCParser
case' = do
  reserved "case"
  t <- term
  optional spaces
  reserved "of"
  optional spaces
  branches <- branch `sepBy` (reservedOp "|")
  pos <- getPosition
  return $ TCase (infoFrom pos) t $ Map.fromList branches
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
  return $ TTag (infoFrom pos) key t ty

fix :: LCParser
fix = do
    reserved "fix"
    t <- term
    pos <- getPosition
    return $ TFix (infoFrom pos) t

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = do
  k <- identifier
  _ <- devider
  v <- val
  return (k,v)

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
baseTypeAnnotation = do
    i <- oneOf ['A'..'Z']
    d <- many $ oneOf ['a'..'z']
    return $ TyID (i:d)

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
