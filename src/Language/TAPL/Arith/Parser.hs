module Language.TAPL.Arith.Parser (parse) where

import Language.TAPL.Arith.Types
import Language.TAPL.Arith.Lexer
import Text.Parsec.Error (ParseError(..))
import Text.Parsec hiding (parse)
import Prelude hiding (succ, pred)

type LCParser = Parsec String () Term

parse :: String -> String -> Either ParseError [Term]
parse code path = runParser arithParser () path code

arithParser :: Parsec String () [Term]
arithParser = do
    ast <- term `sepEndBy` semi
    eof
    return ast

term :: LCParser
term = condition
   <|> isZero
   <|> boolean
   <|> nat
   <|> parens term

boolean :: LCParser
boolean = true <|> false

nat :: LCParser
nat = zero <|> succ <|> pred

true :: LCParser
true = constant "true" TTrue

false :: LCParser
false = constant "false" TFalse

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

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
  return $ TIf x y z

constant :: String -> Term -> LCParser
constant name t = do
  reserved name
  return t

fun :: String -> (Term -> Term) -> LCParser
fun name tm = do
  reserved name
  p <- getPosition
  t <- term
  return $ tm t