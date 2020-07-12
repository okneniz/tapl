module Language.TAPL.Equirec.Parser (parse) where

import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Context
import Language.TAPL.Equirec.Lexer

import Prelude hiding (abs, succ, pred)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.List (findIndex)

type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError (AST, LCNames)
parse = runParser equirecParser []

equirecParser :: Parsec String LCNames (AST, LCNames)
equirecParser = do
    ast <- term `sepEndBy` semi
    eof
    names <- getState
    return (ast, names)

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp pos

notApply :: LCParser
notApply = (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> (parens notApply)

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    _ <- dot
    optional spaces
    context <- getState
    modifyState $ bind varName (VarBind varType)
    t <- term
    setState context
    return $ TAbs p varName varType t

variable :: LCParser
variable = do
    name <- identifier
    ns <- getState
    p <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar p n (length $ ns)
         Nothing -> unexpected $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

termType :: LCTypeParser
termType = do
    _ <- colon
    ty <- typeAnnotation
    return ty

typeAnnotation :: LCTypeParser
typeAnnotation = try recursiveType
             <|> (try arrowAnnotation <?> "arrow type annotation")
             <|> (try notArrowAnnotation)
             <|> (try $ parens typeAnnotation)

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
                    optional spaces
                    reservedOp "->"
                    optional spaces
                    return $ TyArrow

recursiveType :: LCTypeParser
recursiveType = do
    reserved "Rec"
    _ <- spaces
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    _ <- dot
    context <- getState
    setState $ addName context (i:d)
    ty <- typeAnnotation
    return $ TyRec (i:d) ty

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = varOrID <?> "ID type or type variable"

varOrID:: LCTypeParser
varOrID = do
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    names <- getState
    let name = (i:d)
    return $ case findIndex (\(x,_) -> x == name) names of
                  Just x -> TyVar x (length names)
                  Nothing -> TyID name
