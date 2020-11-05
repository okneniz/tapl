module Language.TAPL.Fomega.Lexer where

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
        "lambda",
        "All"
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
brackets      = Token.brackets      lexer
comma         = Token.comma         lexer
semi          = Token.semi          lexer
whiteSpace    = Token.whiteSpace    lexer
dot           = Token.dot           lexer
colon         = Token.colon         lexer
