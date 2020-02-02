{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.SimpleBool.Parser (parse) where

import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Context
import Language.TAPL.SimpleBool.Lexer

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intercalate, all, nub, (\\))
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

type LCParser = Parsec String (SimpleBoolContext Term) Term
type LCTypeParser = Parsec String (SimpleBoolContext Term) Type

parse :: String -> String -> Either ParseError (SimpleBoolContext AST)
parse = runParser simpleBoolParser pureContext
  where pureContext = SimpleBoolContext withoutNames unit
        withoutNames = []
        unit = TTrue $ Info { row = 0, column = 0 }

simpleBoolParser :: Parsec String (SimpleBoolContext Term) (SimpleBoolContext AST)
simpleBoolParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of SimpleBoolContext names _ -> SimpleBoolContext names ast)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

term :: LCParser
term = try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp (infoFrom pos)

notApply :: LCParser
notApply = (boolean     <?> "boolean")
       <|> (condition   <?> "condition")
       <|> (abstraction <?> "abstraction")
       <|> (variable    <?> "variable")
       <|> (parens notApply)

abstraction :: LCParser
abstraction = do
    pos <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    dot
    optional spaces
    context <- getState
    setState $ bind context varName $ (VarBind varType)
    t <- term
    setState context
    return $ TAbs (infoFrom pos) varName varType t

variable :: LCParser
variable = do
    name <- identifier
    context <- getState
    let ns = names context
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar (infoFrom pos) n (length $ ns)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (Info -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- term
    return $ tm (infoFrom p) t

constant :: String -> (Info -> Term) -> LCParser
constant name t = do
    p <- getPosition
    reserved name
    return $ t (infoFrom p)

condition :: LCParser
condition = do
    reserved "if"
    x <- term
    reserved "then"
    y <- term
    reserved "else"
    z <- term
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

termType :: LCTypeParser
termType = do { colon; typeAnnotation }

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation <|> booleanAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (booleanAnnotation <|> parens arrowAnnotation) $ do
    optional spaces
    reservedOp "->"
    optional spaces
    return $ TyArrow

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty
