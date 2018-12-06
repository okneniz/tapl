{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullSimple.Parser where

import TAPL.FullSimple.Types
import TAPL.FullSimple.Context
import TAPL.FullSimple.Lexer

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intercalate, all, nub, (\\))
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

type LCParser = Parsec String (FullSimpleContext Term) Term
type LCTypeParser = Parsec String (FullSimpleContext Term) Type

parse :: String -> String -> Either ParseError (FullSimpleContext AST)
parse = runParser fullSimpleParser pureContext
  where pureContext = FullSimpleContext withoutNames unit
        withoutNames = []
        unit = TUnit $ Info { row = 0, column = 0 }

fullSimpleParser :: Parsec String (FullSimpleContext Term) (FullSimpleContext AST)
fullSimpleParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of FullSimpleContext names _ -> FullSimpleContext names ast)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

padded :: Stream s m Char => String -> ParsecT s u m String
padded x = spaces *> string x <* spaces

terms :: Parsec String (FullSimpleContext Term) [Term]
terms = term `sepEndBy` op
        where  def = []
               op = do
                 padded ";"
                 optionMaybe newline

term :: LCParser
term = try (abstraction <?> "abstraction")
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
       <|> try ((variant value) <?> "variant")
       <|> try (condition <?> "condition")
       <|> try (let' <?> "let")
       <|> try (fix <?> "fix")
       <|> try (case' <?> "case")
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)

value :: LCParser
value = optionalAscribed $ try (boolean <?> "boolean")
                       <|> try (string' <?> "string")
                       <|> try (succ <?> "succ")
                       <|> try (pred <?> "pred")
                       <|> try (isZero <?> "zero?")
                       <|> try (zero <?> "zero")
                       <|> try (float <?> "float")
                       <|> try (integer <?> "integer")
                       <|> try (unit <?> "unit")
                       <|> try (record <?> "record")
                       <|> try (pair <?> "pair")

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    dot
    optional spaces
    context <- getState
    setState $ bind context varName $ (VarBind varType)
    t <- term
    setState context
    return $ TAbs (infoFrom pos) varName varType t

variable :: LCParser
variable = optionalAscribed $ lookup' (try integer <|> try keyword) $ do
    name <- identifier
    context <- getState
    let ns = names context
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar (infoFrom pos) n (length $ ns)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

boolean :: LCParser
boolean = try true <|> false

string' :: LCParser
string' = do
    char '\"'
    s <- identifier
    char '\"'
    pos <- getPosition
    return $ TString (infoFrom pos) s

unit :: LCParser
unit = do
    string "unit"
    pos <- getPosition
    return $ TUnit (infoFrom pos)

nat :: LCParser
nat = zero <|> succ <|> pred

succ :: LCParser
succ = do
    padded "succ"
    t <- term
    pos <- getPosition
    return $ TSucc (infoFrom pos) t

pred :: LCParser
pred = do
    padded "pred"
    t <- term
    pos <- getPosition
    return $ TPred (infoFrom pos) t

zero :: LCParser
zero = do
    reserved "zero"
    pos <- getPosition
    return $ TZero (infoFrom pos)

isZero :: LCParser
isZero = do
    padded "zero?"
    t <- term
    pos <- getPosition
    return $ TIsZero (infoFrom pos) t

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

true :: LCParser
true = do
    padded "true"
    pos <- getPosition
    return $ TTrue (infoFrom pos)

false :: LCParser
false = do
    padded "false"
    pos <- getPosition
    return $ TFalse (infoFrom pos)

condition :: LCParser
condition = do
    padded "if"
    x <- term
    padded "then"
    y <- term
    padded "else"
    z <- term
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

lookup' :: LCParser -> LCParser -> LCParser
lookup' key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
  where dotRef key t = do
          _ <- char '.'
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
          _ <- string "as"
          optional spaces
          ty <- typeAnnotation
          pos <- getPosition
          return $ TAscribe (infoFrom pos) t ty

pair :: LCParser
pair = lookup' integer $ braces $ do
    t1 <- term
    padded ","
    t2 <- term
    pos <- getPosition
    return $ TPair (infoFrom pos) t1 t2

record :: LCParser
record = lookup' keyword $ braces $ do
    ts <- (keyValue '=') `sepBy` (padded ",")
    pos <- getPosition
    return $ TRecord (infoFrom pos) ts

keyword :: LCParser
keyword = do
  word <- identifier
  p <- getPosition
  return $ TKeyword (infoFrom p) word

keyValue :: Char -> Parsec String (FullSimpleContext Term) (String, Term)
keyValue c = do
     k <- identifier
     char c
     v <- term
     return (k, v)

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
    return $ TLet (infoFrom p) v t1 t2

case' :: LCParser
case' = do
  reserved "case"
  t <- term
  optional spaces
  reserved "of"
  optional spaces
  branches <- branch `sepBy` (padded "|")
  pos <- getPosition
  return $ TCase (infoFrom pos) t branches
  where branch = do
          (caseName, varName) <- pattern
          reserved "->"
          context <- getState
          modifyState $ \c -> addName c varName
          t2 <- term
          setState context
          return (caseName, varName, t2)
        pattern = angles $ do
          caseName <- identifier
          padded "="
          varName <- identifier
          return (caseName, varName)

variant :: LCParser -> LCParser
variant x = do
  padded "<"
  pos <- getPosition
  key <- identifier
  padded "="
  t <- x
  padded ">"
  reserved "as"
  ty <- variantAnnotation
  return $ TTag (infoFrom pos) key t ty

fix :: LCParser
fix = do
    reserved "fix"
    t <- term
    pos <- getPosition
    return $ TFix (infoFrom pos) t

termType :: LCTypeParser
termType = do
    char ':'
    typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation
             <|> try productAnnotation
             <|> try recordAnnotation
             <|> try variantAnnotation
             <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    padded "->"
    return $ TyArrow

productAnnotation :: LCTypeParser
productAnnotation = braces $ chainl1 typeAnnotation $ do
    padded "*"
    return $ TyProduct

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = try booleanAnnotation
                 <|> try stringAnnotation
                 <|> try unitAnnotation
                 <|> try natAnnotation
                 <|> try floatAnnotation
                 <|> try baseTypeAnnotation
                 <|> try topAnnotation
                 <|> try botAnnotation

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
    tys <- keyValue2 `sepBy` comma
    return $ TyRecord tys

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    string name
    return ty

keyValue2 :: Parsec String (FullSimpleContext Term) (String, Type)
keyValue2 = do
    k <- identifier
    colon
    v <- typeAnnotation
    return (k, v)

variantAnnotation :: LCTypeParser
variantAnnotation = do
    string "<"
    ts <- (keyType ':') `sepBy` (padded ",")
    string ">"
    return $ TyVariant ts

keyType :: Char -> Parsec String (FullSimpleContext Term) (String, Type)
keyType c = do
     k <- identifier
     char c
     v <- typeAnnotation
     return (k, v)
