module Language.TAPL.TypedArith.Parser (parse) where

import Language.TAPL.TypedArith.Types
import Language.TAPL.TypedArith.Context
import Language.TAPL.TypedArith.Lexer

import Prelude hiding (abs, succ, pred)

import Text.Parsec hiding (parse)
import Text.Parsec.Prim (try)

import Data.List (findIndex)

type LCParser = Parsec String LCNames Term
type LCTypeParser = Parsec String LCNames Type

parse :: String -> String -> Either ParseError (AST, LCNames)
parse = runParser typedArithParser []

typedArithParser :: Parsec String LCNames (AST, LCNames)
typedArithParser = do
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
    _ <- dot
    optional spaces
    context <- getState
    modifyState $ bind varName (VarBind varType)
    t <- term
    setState context
    return $ TAbs pos varName varType t

variable :: LCParser
variable = do
    name <- identifier
    ns <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) ns of
         Just n -> return $ TVar pos n (length $ ns)
         Nothing -> error $ "variable " ++ show name ++ " has't been bound in context " ++ " " ++ (show pos)

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

fun :: String -> (SourcePos -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- term
    return $ tm p t

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

isZero :: LCParser
isZero = fun "zero?" TIsZero

constant :: String -> (SourcePos -> Term) -> LCParser
constant name t = do
    p <- getPosition
    reserved name
    return $ t p

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
    return $ TIf pos x y z

termType :: LCTypeParser
termType = colon >> typeAnnotation

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
primitiveType name ty = reserved name >> return ty
