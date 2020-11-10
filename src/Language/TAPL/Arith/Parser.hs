module Language.TAPL.Arith.Parser (parse) where

import Language.TAPL.Arith.Types
import Language.TAPL.Arith.Lexer
import Text.Parsec.Error (ParseError)
import Text.Parsec hiding (parse)
import Prelude hiding (succ, pred)
import Data.Functor (($>))

type LCParser = Parsec String () Term

parse :: String -> String -> Either ParseError [Term]
parse code path = runParser arithParser () path code

arithParser :: Parsec String () [Term]
arithParser = term `sepEndBy` semi <* eof

term :: LCParser
term = value <|> isZero <|> condition <|> parens term

value :: LCParser
value = boolean <|> nat

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

nat :: LCParser
nat = succ <|> pred <|> zero <|> integer
    where succ = fun "succ" TSucc
          pred = fun "pred" TPred
          zero = constant "zero" TZero
          integer = do
            i <- try natural
            toNat i TZero
          toNat i _ | i < 0 = unexpected "unexpected negative number"
          toNat 0 t = return t
          toNat i t = toNat (i - 1) (TSucc t)

isZero :: LCParser
isZero = fun "zero?" TIsZero

condition :: LCParser
condition = TIf <$> (reserved "if"   *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

constant :: String -> Term -> LCParser
constant name t = reserved name $> t

fun :: String -> (Term -> Term) -> LCParser
fun name tm = tm <$> (reserved name >> term)
