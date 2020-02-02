module Language.TAPL.TypedArith.Lexer where

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
        "if",
        "then",
        "else",
        "true",
        "false",
        "lambda",
        "zero",
        "Bool",
        "Nat",
        "succ",
        "pred",
        "zero?"
    ],
    Token.reservedOpNames = [
        "->"
    ]
}

lexer = Token.makeTokenParser languageDefinition

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
comma         = Token.comma         lexer
semi          = Token.semi          lexer
whiteSpace    = Token.whiteSpace    lexer
dot           = Token.dot           lexer
colon         = Token.colon         lexer