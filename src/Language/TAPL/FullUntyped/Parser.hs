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
parse = runParser fullUntypedParser []

fullUntypedParser :: Parsec String LCNames ([Command], LCNames)
fullUntypedParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = evalCommand

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
       <|> (variable <?> "variable")
       <|> try (parens notApply)

value :: LCParser
value = (boolean <?> "boolean")
    <|> (unit <?> "unit")
    <|> (stringT <?> "string")
    <|> (float <?> "float")
    <|> (nat <?> "nat")
    <|> (abstraction <?> "abstraction")
    <|> ((optionalProjection identifier record) <?> "record")
    <|> ((optionalProjection pairIndexes pair) <?> "pair")

isZero :: LCParser
isZero = fun "zero?" TIsZero

abstraction :: LCParser
abstraction = optionalParens $ do
    pos <- getPosition
    name <-  reserved "lambda" *> identifier
    names <- getState
    modifyState $ addName name
    t <- dot *> term
    setState names
    return $ TAbs pos name t

variable :: LCParser
variable = optionalProjection (pairIndexes <|> identifier) $ do
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

timesFloat :: LCParser
timesFloat = TTimesFloat <$> (reserved "timesfloat" *> getPosition) <*> notApply <*> (spaces *> notApply)

keyValue :: Parsec String u a -> Parsec String u b -> Parsec String u (String, b)
keyValue devider val = (,) <$> (identifier <* devider) <*> val

optionalParens :: Parsec String u a -> Parsec String u a
optionalParens f = try (parens f) <|> f
