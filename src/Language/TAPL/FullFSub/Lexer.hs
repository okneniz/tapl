module Language.TAPL.FullFSub.Lexer where

import Text.Parsec (alphaNum, lower, oneOf, (<|>))
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
        "Int",
        "Unit",
        "succ",
        "pred",
        "zero?",
        "let",
        "as",
        "in",
        "fix",
        "timesfloat",
        "All",
        "Some"
    ],
    Token.reservedOpNames = [
        "=",
        "*"
    ]
}

lexer = Token.makeTokenParser languageDefinition

identifier    = Token.identifier    lexer
reserved      = Token.reserved      lexer
reservedOp    = Token.reservedOp    lexer
parens        = Token.parens        lexer
braces        = Token.braces        lexer
brackets      = Token.brackets      lexer
comma         = Token.comma         lexer
floatNum      = Token.float         lexer
natural       = Token.natural       lexer
semi          = Token.semi          lexer
whiteSpace    = Token.whiteSpace    lexer
dot           = Token.dot           lexer
colon         = Token.colon         lexer
stringLiteral = Token.stringLiteral lexer