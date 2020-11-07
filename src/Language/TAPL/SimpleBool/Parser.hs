module Language.TAPL.SimpleBool.Parser (parse) where

import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Context
import Language.TAPL.SimpleBool.Lexer
import Language.TAPL.Common.Helpers (padded)

import Data.Functor (($>))

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)
import Data.List (findIndex)

type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError (AST, LCNames)
parse = runParser simpleBoolParser []

simpleBoolParser :: Parsec String LCNames (AST, LCNames)
simpleBoolParser = (,) <$> (term `sepEndBy` semi <* eof) <*> getState

term :: LCParser
term = apply <|> notApply <|> parens term

apply :: LCParser
apply = try $ chainl1 notApply $ TApp <$> getPosition

notApply :: LCParser
notApply = (boolean     <?> "boolean")
       <|> (condition   <?> "condition")
       <|> (abstraction <?> "abstraction")
       <|> (variable    <?> "variable")
       <|> (parens notApply)

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    varName <- reserved "lambda" *> identifier
    varType <- colon *> typeAnnotation <* dot
    context <- getState
    modifyState $ addVar varName varType
    t <- term
    setState context
    return $ TAbs pos varName varType t

variable :: LCParser
variable = do
    name <- identifier
    ns <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar pos n (length ns)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context " <> " " <> (show pos)

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = reserved name >> (t <$> getPosition)

condition :: LCParser
condition = TIf <$> getPosition
                <*> (reserved "if" *> term)
                <*> (reserved "then" *> term)
                <*> (reserved "else" *> term)

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation <|> booleanAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (booleanAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") $> TyArrow

booleanAnnotation :: LCTypeParser
booleanAnnotation = reserved "Bool" >> return TyBool
