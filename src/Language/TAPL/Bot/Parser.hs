module Language.TAPL.Bot.Parser (parse) where

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context
import Language.TAPL.Bot.Lexer
import Language.TAPL.Common.Context (findVarName)
import Language.TAPL.Common.Helpers (ucid)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

type LCCommandParser = Parsec String LCNames Command
type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError ([Command], LCNames)
parse = runParser botParser []

botParser :: Parsec String LCNames ([Command], LCNames)
botParser = do
    commands <- command `sepEndBy` semi
    eof
    names <- getState
    return $ (commands, names)

command :: Parsec String LCNames Command
command =  (try bindCommand) <|> (try evalCommand)

bindCommand :: LCCommandParser
bindCommand = do
    pos <- getPosition
    x <- ucid <* spaces
    reservedOp "="
    modifyState $ addName x
    ty <- typeAnnotation
    return $ Bind pos x $ TypeAddBind ty

evalCommand :: LCCommandParser
evalCommand = try $ Eval <$> term `sepEndBy` semi

term :: LCParser
term = (try apply <?> "apply")
    <|> (try notApply <?> "not apply expressions")
    <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
          p <- getPosition
          whiteSpace
          return $ TApp p

notApply :: LCParser
notApply =  try abstraction
        <|> try variable
        <|> parens notApply

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    _ <- dot
    optional space
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
        Just n -> return $ TVar p n (length $ ns)
        Nothing -> unexpected $ "variable " ++ name ++ " has't been bound in context " ++ (show p)

termType :: LCTypeParser
termType = colon >> typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation
             <|> try notArrowAnnotation
             <|> (try $ parens typeAnnotation)

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
                    reservedOp "->"
                    return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = topAnnotation <|> botAnnotation

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot
