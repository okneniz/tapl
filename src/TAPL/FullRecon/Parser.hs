{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullRecon.Parser (parse) where

import TAPL.FullRecon.Types
import TAPL.FullRecon.Context
import TAPL.FullRecon.Lexer

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec hiding (parse)
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, foldl1, intercalate, all, nub, (\\), partition)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

type LCParser = Parsec String Names Term
type LCTypeParser = Parsec String Names Type

parse :: String -> String -> Either ParseError (Names, [Term])
parse = runParser parser []

parser :: Parsec String Names (Names, [Term])
parser = do
    ast <- term `sepEndBy` semi
    eof
    names <- getState
    return $ (names, ast)

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

term :: LCParser
term = try typeBinding
   <|> try apply
   <|> try notApply
   <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
            optional spaces
            pos <- getPosition
            return $ TApp (infoFrom pos)

notApply :: LCParser
notApply = try value
       <|> try (condition <?> "condition")
       <|> try (abstraction <?> "abstraction")
       <|> try (let' <?> "let")
       <|> try (variable <?> "variable")
       <|> try (parens notApply)
       <|> try (parens apply)

typeBinding :: LCParser
typeBinding = do
    pos <- getPosition
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    spaces
    reserved "="
    modifyState $ \c -> addName c (i:d)
    ty <- typeAnnotation
    return $ TBind (infoFrom pos) (i:d) $ VarBind ty

notTypeBind :: LCParser
notTypeBind = try apply
          <|> try notApply
          <|> try (parens notTypeBind)

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
    name <- identifier
    ty <- optionMaybe termType
    dot
    optional spaces
    names <- getState
    setState $ addName names name
    t <- notTypeBind
    setState names
    return $ TAbs (infoFrom pos) name ty t

let' :: LCParser
let' = do
    reserved "let"
    p <- getPosition
    v <- identifier
    reservedOp "="
    t1 <- term
    optional spaces
    reserved "in"
    optional spaces
    context <- getState
    modifyState $ \c -> addName c v
    t2 <- term
    return $ TLet (infoFrom p) v t1 t2

variable :: LCParser
variable = do
    name <- identifier
    names <- getState
    pos <- getPosition
    case findVarName names name of
         Just n -> return $ TVar (infoFrom pos) n (depth names)
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
    t <- notTypeBind
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
    x <- notTypeBind
    reserved "then"
    y <- notTypeBind
    reserved "else"
    z <- notTypeBind
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

termType :: LCTypeParser
termType = do
    colon
    typeAnnotation

typeAnnotation :: LCTypeParser
typeAnnotation = try arrowAnnotation
             <|> notArrowAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
    optional spaces
    reservedOp "->"
    optional spaces
    return $ TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = booleanAnnotation
                 <|> try natAnnotation
                 <|> try typeVarOrID

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty

typeVarOrID:: LCTypeParser
typeVarOrID = do
    i <- try $ oneOf ['A'..'Z']
    d <- try $ many $ oneOf ['a'..'z']
    names <- getState
    let name = (i:d)
    return $ case findVarName names name of
                  Just varName -> TyVar varName (depth names)
                  Nothing -> TyID name
