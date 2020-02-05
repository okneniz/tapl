module Language.TAPL.Bot.Parser (parse) where

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context
import Language.TAPL.Bot.Lexer

import Prelude hiding (abs, succ, pred)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.List (findIndex)

type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError (AST, LCNames)
parse = runParser botParser []

botParser :: Parsec String LCNames (AST, LCNames)
botParser = do
    ast <- term `sepEndBy` semi
    eof
    names <- getState
    return (ast, names)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

term :: LCParser
term = (try apply <?> "apply")
    <|> (try notApply <?> "not apply expressions")
    <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
          p <- getPosition
          whiteSpace
          return $ TApp (infoFrom p)

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
    modifyState $ bind varName (VarBind varType)
    t <- term
    setState context
    return $ TAbs (infoFrom p) varName varType t

variable :: LCParser
variable = do
    name <- identifier
    p <- getPosition
    ns <- getState
    case findIndex ((== name) . fst) ns of
        Just n -> return $ TVar (infoFrom p) n (length $ ns)
        Nothing -> unexpected $ "variable " ++ name ++ " has't been bound in context " ++ (show p)

termType :: LCTypeParser
termType = do
    _ <- colon
    ty <- typeAnnotation
    return ty

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation
             <|> try notArrowAnnotation
             <|> (try $ parens typeAnnotation)

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
                    reserved "->"
                    return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = topAnnotation <|> botAnnotation

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = reserved name >> return ty

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot
