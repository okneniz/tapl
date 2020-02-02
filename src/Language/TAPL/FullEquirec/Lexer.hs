module Language.TAPL.FullEquirec.Lexer where

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
        "unit",
        "zero",
        "String",
        "Bool",
        "Nat",
        "Float",
        "Unit",
        "Top",
        "Bot",
        "Int",
        "Unit",
        "succ",
        "pred",
        "zero?",
        "ref",
        "let",
        "as",
        "in",
        "case",
        "of",
        "fix",
        "Rec",
        "timesfloat"
    ],
    Token.reservedOpNames = [
        "*",
        "=",
        "|",
        "->"
    ]
}

lexer = Token.makeTokenParser languageDefinition

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
braces        = Token.braces        lexer
angles        = Token.angles        lexer
comma         = Token.comma         lexer
floatNum      = Token.float         lexer
natural       = Token.natural       lexer
semi          = Token.semi          lexer
whiteSpace    = Token.whiteSpace    lexer
dot           = Token.dot           lexer
colon         = Token.colon         lexer
stringLiteral = Token.stringLiteral lexer