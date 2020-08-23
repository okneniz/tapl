module Language.TAPL.Arith.Parser (parse) where

import Language.TAPL.Arith.Types
import Language.TAPL.Arith.Lexer
import Text.Parsec.Error (ParseError)
import Text.Parsec hiding (parse)
import Prelude hiding (succ, pred)

type LCParser = Parsec String () Term

parse :: String -> String -> Either ParseError [Term]
parse code path = runParser arithParser () path code

arithParser :: Parsec String () [Term]
arithParser = term `sepEndBy` semi <* eof

term :: LCParser
term = condition
   <|> isZero
   <|> boolean
   <|> nat
   <|> parens term

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

nat :: LCParser
nat = zero <|> succ <|> pred

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

zero :: LCParser
zero = constant "zero" TZero

condition :: LCParser
condition = TIf <$> (reserved "if"   *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

constant :: String -> Term -> LCParser
constant name t = reserved name >> return t

fun :: String -> (Term -> Term) -> LCParser
fun name tm = pure tm <*> (reserved name >> term)