module Language.TAPL.Bot.Parser (parse) where

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context
import Language.TAPL.Bot.Lexer
import Language.TAPL.Common.Context (findVarName)
import Language.TAPL.Common.Helpers (ucid, padded)

import Data.Functor (($>))

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser botParser []

botParser :: Parsec String LCNames ([Command], LCNames)
botParser = (,) <$> (command `sepEndBy` semi <* eof) <*> getState

command :: Parsec String LCNames Command
command = bindCommand <|> evalCommand

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces
    reservedOp "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = Eval <$> term `sepEndBy` semi

term :: LCParser
term = apply <|> notApply <|> parens term

apply :: LCParser
apply = try $ chainl1 (notApply <|> try (parens apply)) $ TApp <$> getPosition

notApply :: LCParser
notApply = abstraction <|> variable <|> parens notApply

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- colon *> typeAnnotation <* dot
    context <- getState
    modifyState $ addVar varName varType
    t <- term
    setState context
    return $ TAbs p varName varType t

variable :: LCParser
variable = do
    name <- identifier
    p <- getPosition
    ns <- getState
    case findVarName ns name of
        Just n -> return $ TVar p n (length ns)
        Nothing -> unexpected $ "variable " <> name <> " has't been bound in context " <> (show p)

typeAnnotation :: LCTypeParser
typeAnnotation = arrowAnnotation <|> notArrowAnnotation <|> parens typeAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ padded (reservedOp "->") $> TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = topAnnotation <|> botAnnotation

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name $> ty

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot
