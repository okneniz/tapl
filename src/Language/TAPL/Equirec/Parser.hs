{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.Equirec.Parser (parse) where

import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Context
import Language.TAPL.Equirec.Lexer

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Prelude hiding (succ, pred, lookup)
import qualified Prelude (lookup)

import Data.List (findIndex)

type LCParser = Parsec String (EquirecContext Term) Term
type LCTypeParser = Parsec String (EquirecContext Term) Type

parse :: String -> String -> Either ParseError (EquirecContext AST)
parse code path = runParser equirecParser pureContext path code
          where pureContext = EquirecContext withoutNames noop
                withoutNames = []
                noop = TVar Nothing (-1) (-1)

parseFile :: String -> IO (Either ParseError (EquirecContext AST))
parseFile path = do
    code <- readFile path
    return $ parse code path

equirecParser :: Parsec String (EquirecContext Term) (EquirecContext AST)
equirecParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of EquirecContext names _ -> EquirecContext names ast)

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term


apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp (Just pos)

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
    dot
    optional spaces
    context <- getState
    setState $ bind context varName $ (VarBind varType)
    t <- term
    setState context
    return $ TAbs (Just p) varName varType t

variable :: LCParser
variable = do
    name <- identifier
    context <- getState
    let ns = names context
    p <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar (Just p) n (length $ ns)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show p)

-- Types annotations --

termType :: LCTypeParser
termType = do
    colon
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
    spaces
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    dot
    context <- getState
    setState $ addName context (i:d)
    ty <- typeAnnotation
    return $ TyRec (i:d) ty

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = (varOrID <?> "ID type or type variable")

varOrID:: LCTypeParser
varOrID = do
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    context <- getState
    let name = (i:d)
    return $ case varName context name of
                  Just x -> TyVar x (length $ names context)
                  Nothing -> TyID name
