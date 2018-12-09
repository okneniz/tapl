module TAPL.Arith.Lexer (reserved, parens, semi) where

import Text.Parsec (letter, alphaNum, lower, oneOf, (<|>))
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDefinition = emptyDef {
    Token.commentStart    = "/*",
    Token.commentEnd      = "*/",
    Token.commentLine     = "//",
    Token.identStart      = lower,
    Token.identLetter     = (alphaNum <|> oneOf "?_"),
    Token.reservedNames   = [
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

lexer = Token.makeTokenParser languageDefinition

reserved   = Token.reserved   lexer
parens     = Token.parens     lexer
semi       = Token.semi       lexer
