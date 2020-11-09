module Language.TAPL.Untyped.Parser (parse) where

import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Context
import Language.TAPL.Untyped.Lexer
import Language.TAPL.Common.Helpers (withState)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)
import Data.List (findIndex)

type LCParser = Parsec String LCNames Term

parse :: String -> String -> Either ParseError ([Term], LCNames)
parse = runParser untypedParser []

untypedParser :: Parsec String LCNames ([Term], LCNames)
untypedParser = (,) <$> (term `sepEndBy` semi <* eof) <*> getState

term :: LCParser
term = apply <|> notApply <|> parens term

apply :: LCParser
apply = try $ chainl1 notApply $ TApp <$> getPosition

notApply :: LCParser
notApply = (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> (parens notApply)

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    varName <- identifier <* dot
    withState (addName varName) $ TAbs pos varName <$> term

variable :: LCParser
variable = do
    name <- identifier
    ns <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar pos n (length ns)
         Nothing -> unexpected $ "variable " <> show name <> " has't been bound in context " <> " " <> (show pos)
