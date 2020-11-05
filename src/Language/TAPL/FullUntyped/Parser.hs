module Language.TAPL.FullUntyped.Parser (parse) where

import Language.TAPL.FullUntyped.Types
import Language.TAPL.FullUntyped.Context
import Language.TAPL.FullUntyped.Lexer

import Prelude hiding (abs, succ, pred)
import qualified Data.Map.Lazy as Map
import Data.List (findIndex)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser untypedParser []

untypedParser :: Parsec String LCNames ([Command], LCNames)
untypedParser = (,) <$> (evalCommand `sepEndBy` semi <* eof) <*> getState

evalCommand :: LCCommandParser
evalCommand = try $ Eval <$> term `sepEndBy` semi

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = try value
       <|> try (timesFloat <?> "timesfloat")
       <|> try (condition <?> "condition")
       <|> try (let' <?> "let")
       <|> try (abstraction <?> "abstraction")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)

value :: LCParser
value = (boolean <?> "boolean")
    <|> (string' <?> "string")
    <|> (succ <?> "succ")
    <|> (pred <?> "pred")
    <|> (isZero <?> "zero?")
    <|> (zero <?> "zero")
    <|> (unit <?> "unit")
    <|> try (float <?> "float")
    <|> try (record <?> "record")
    <|> try (pair <?> "pair")

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    _ <- dot
    optional spaces
    context <- getState
    modifyState $ addVar varName
    t <- term
    setState context
    return $ TAbs p varName t

variable :: LCParser
variable = optionalProjection (pairIndexes <|> identifier) $ do
    name <- identifier
    names <- getState
    p <- getPosition
    case findIndex ((== name) . fst) names of
         Just n -> return $ TVar p n (length names)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

pairIndexes :: Parsec String LCNames String
pairIndexes = flip(:) [] <$> oneOf "01"

string' :: LCParser
string' = TString <$> getPosition <*> try stringLiteral

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

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

pair :: LCParser
pair = optionalProjection pairIndexes $ braces $ TPair <$> getPosition <*> (term <* comma) <*> term

optionalProjection :: Parsec String LCNames String -> LCParser -> LCParser
optionalProjection key tm = do
    t <- tm
    t' <- (try $ dotRef key t) <|> (return t)
    return t'
    where dotRef k t = do
            _ <- dot
            pos <- getPosition
            i <- k
            t' <- (try $ dotRef key (TProj pos t i)) <|> (return $ TProj pos t i)
            return t'

record :: LCParser
record = optionalProjection identifier $ braces $ do
    ts <- keyValue (reservedOp "=") term `sepBy` comma
    p <- getPosition
    return $ TRecord p $ Map.fromList ts

let' :: LCParser
let' = do
    reserved "let"
    p <- getPosition
    name <- identifier
    reservedOp "="
    t1 <- term
    optional spaces
    reserved "in"
    optional spaces
    modifyState $ addName name
    t2 <- term
    return $ TLet p name t1 t2

timesFloat :: LCParser
timesFloat = try $ do
    reserved "timesfloat"
    pos <- getPosition
    t1 <- notApply
    spaces
    t2 <- notApply
    return $ TTimesFloat pos t1 t2

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val
