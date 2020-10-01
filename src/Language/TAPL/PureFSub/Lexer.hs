module Language.TAPL.PureFSub.Lexer where

import Data.Functor.Identity (Identity)
import Text.Parsec (alphaNum, lower, oneOf, (<|>), Parsec)
import Text.ParserCombinators.Parsec.Language (LanguageDef, emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDefinition :: LanguageDef st
languageDefinition = emptyDef {
    Token.commentStart    = "/*",
    Token.commentEnd      = "*/",
    Token.commentLine     = "//",
    Token.identStart      = lower,
    Token.identLetter     = (alphaNum <|> oneOf "?_"),
    Token.reservedNames   = [
        "lambda",
        "Top"
    ],
    Token.reservedOpNames = [
        "=",
        "->",
        "<:"
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