{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullEquirec.Parser (parse) where

import Language.TAPL.FullEquirec.Types
import Language.TAPL.FullEquirec.Names
import Language.TAPL.FullEquirec.Lexer

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, foldl1, intercalate, all, nub, (\\), partition)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

type LCParser = Parsec String Names Term
type LCTypeParser = Parsec String Names Type

parse :: String -> String -> Either ParseError (Names, [Term])
parse = runParser parser []

parser :: Parsec String Names (Names, [Term])
parser = do
    ast <- term `sepEndBy` semi
    eof
    names <- getState
    return $ (names, ast)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

term :: LCParser
term = try typeBinding
   <|> try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp (infoFrom pos)

notApply :: LCParser
notApply = try value
       <|> try (timesFloat <?> "times float")
       <|> try ((variant value) <?> "variant")
       <|> try (condition <?> "condition")
       <|> try (let' <?> "let")
       <|> try (fix <?> "fix")
       <|> try (case' <?> "case")
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)
       <|> try (parens apply)

typeBinding :: LCParser
typeBinding = do
    pos <- getPosition
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    spaces
    reserved "="
    modifyState $ \c -> addName c (i:d)
    ty <- typeAnnotation
    return $ TBind (infoFrom pos) (i:d) $ TypeAddBind ty

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
    setState $ addVar names name ty
    t <- notTypeBind
    setState names
    return $ TAbs (infoFrom pos) name ty t

variable :: LCParser
variable = optionalAscribed $ lookup' (integer <|> keyword) $ do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar (infoFrom pos) n (depth names)
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

nat :: LCParser
nat = zero <|> succ <|> pred

fun :: String -> (Info -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- notTypeBind
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
    x <- notTypeBind
    reserved "then"
    y <- notTypeBind
    reserved "else"
    z <- notTypeBind
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

lookup' :: LCParser -> LCParser -> LCParser
lookup' key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
  where dotRef key t = do
          dot
          pos <- getPosition
          i <- key
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
    t1 <- notTypeBind
    comma
    t2 <- notTypeBind
    pos <- getPosition
    return $ TPair (infoFrom pos) t1 t2

record :: LCParser
record = lookup' keyword $ braces $ do
    ts <- (keyValue (reservedOp "=") term) `sepBy` comma
    pos <- getPosition
    return $ TRecord (infoFrom pos) ts

keyword :: LCParser
keyword = do
  word <- identifier
  p <- getPosition
  return $ TKeyword (infoFrom p) word

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
    modifyState $ \c -> addName names name
    t2 <- notTypeBind
    return $ TLet (infoFrom p) name t1 t2

case' :: LCParser
case' = do
  reserved "case"
  t <- notTypeBind
  optional spaces
  reserved "of"
  optional spaces
  branches <- branch `sepBy` (reservedOp "|")
  pos <- getPosition
  return $ TCase (infoFrom pos) t branches
  where branch = do
          (caseName, name) <- pattern
          reservedOp "->"
          names <- getState
          modifyState $ \c -> addName names name
          t2 <- notTypeBind
          setState names
          return (caseName, name, t2)
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
  return $ TTag (infoFrom pos) key t ty

fix :: LCParser
fix = do
    reserved "fix"
    t <- notTypeBind
    pos <- getPosition
    return $ TFix (infoFrom pos) t

timesFloat :: LCParser
timesFloat = do
    reserved "timesfloat"
    pos <- getPosition
    t1 <- notApply
    spaces
    t2 <- notApply
    return $ TTimesFloat (infoFrom pos) t1 t2

keyValue devider val = do
  key <- identifier
  devider
  value <- val
  return (key,value)

termType :: LCTypeParser
termType = do
    colon
    typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try recursiveType
             <|> try arrowAnnotation
             <|> notArrowAnnotation

recursiveType :: LCTypeParser
recursiveType = do
    reserved "Rec"
    spaces
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    dot
    names <- getState
    modifyState $ \c -> addName c (i:d)
    ty <- typeAnnotation
    setState names
    return $ TyRec (i:d) ty

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
    return $ TyRecord tys

variantAnnotation :: LCTypeParser
variantAnnotation = angles $ do
    ts <- (keyValue colon typeAnnotation) `sepBy` comma
    return $ TyVariant ts

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty

typeVarOrID:: LCTypeParser
typeVarOrID = do
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    names <- getState
    let name = (i:d)
    return $ case findVarName names name of
                  Just varName -> TyVar varName (depth names)
                  Nothing -> TyID name