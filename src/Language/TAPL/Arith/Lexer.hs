module Language.TAPL.Arith.Lexer (reserved, parens, semi) where

import Data.Functor.Identity (Identity)
import Text.Parsec (alphaNum, lower, oneOf, (<|>), Parsec)
import Text.ParserCombinators.Parsec.Language (LanguageDef, emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDefinition :: LanguageDef st
languageDefinition = emptyDef {
    Token.commentStart  = "/*",
    Token.commentEnd    = "*/",
    Token.commentLine   = "//",
    Token.identStart    = lower,
    Token.identLetter   = (alphaNum <|> oneOf "?_"),
    Token.reservedNames = [
        "true",
        "false",
        "zero?",
        "succ",
        "pred",
        "zero",
        "if",
        "then",
        "else"
    ]
}

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDefinition

reserved :: String -> Parsec String u ()
reserved = Token.reserved lexer

parens :: Parsec String u a -> Parsec String u a
parens = Token.parens lexer

semi :: Parsec String u String
semi = Token.semi lexer
