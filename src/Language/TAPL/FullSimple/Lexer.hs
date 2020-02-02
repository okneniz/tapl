module Language.TAPL.FullSimple.Lexer where

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
        "fix"
    ],
    Token.reservedOpNames = [
        "*",
        "=",
        "|",
        "->"
    ]
}

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser languageDefinition

identifier :: Parsec String u String
identifier = Token.identifier lexer

reserved :: String -> Parsec String u ()
reserved = Token.reserved lexer

reservedOp :: String -> Parsec String u ()
reservedOp = Token.reservedOp    lexer

parens :: Parsec String u a -> Parsec String u a
parens = Token.parens lexer

braces :: Parsec String u a -> Parsec String u a
braces = Token.braces        lexer

angles :: Parsec String u a -> Parsec String u a
angles = Token.angles        lexer

comma :: Parsec String u String
comma = Token.comma lexer

floatNum :: Parsec String u Double
floatNum = Token.float lexer

natural :: Parsec String u Integer
natural = Token.natural lexer

semi :: Parsec String u String
semi = Token.semi lexer

whiteSpace :: Parsec String u ()
whiteSpace = Token.whiteSpace lexer

dot :: Parsec String u String
dot = Token.dot lexer

colon :: Parsec String u String
colon = Token.colon lexer

stringLiteral :: Parsec String u String
stringLiteral = Token.stringLiteral lexer
