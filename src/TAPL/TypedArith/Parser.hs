{-# LANGUAGE FlexibleContexts #-}

module TAPL.TypedArith.Parser where

import TAPL.TypedArith.Types
import TAPL.TypedArith.Context
import TAPL.TypedArith.Lexer

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intercalate, all, nub, (\\))
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

type LCParser = Parsec String (TypedArithContext Term) Term
type LCTypeParser = Parsec String (TypedArithContext Term) Type

parse :: String -> String -> Either ParseError (TypedArithContext AST)
parse = runParser typedArithParser pureContext
  where pureContext = TypedArithContext withoutNames unit
        withoutNames = []
        unit = TTrue $ Info { row = 0, column = 0 }

typedArithParser :: Parsec String (TypedArithContext Term) (TypedArithContext AST)
typedArithParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of TypedArithContext names _ -> TypedArithContext names ast)

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
notApply = value
       <|> (condition <?> "condition")
       <|> (abstraction <?> "abstraction")
       <|> (variable <?> "variable")
       <|> (parens notApply)

value :: LCParser
value = (boolean <?> "boolean")
    <|> (succ <?> "succ")
    <|> (pred <?> "pred")
    <|> (isZero <?> "zero?")
    <|> (zero <?> "zero")

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

nat :: LCParser
nat = zero <|> succ <|> pred

fun :: String -> (Info -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- term
    return $ tm (infoFrom p) t

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

constant :: String -> (Info -> Term) -> LCParser
constant name t = do
    p <- getPosition
    reserved name
    return $ t (infoFrom p)

zero :: LCParser
zero = constant "zero" TZero

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
termType = do
    colon
    typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    optional spaces
    reservedOp "->"
    optional spaces
    return $ TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation <|> natAnnotation

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty
