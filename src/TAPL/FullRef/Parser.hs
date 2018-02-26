{-# LANGUAGE FlexibleInstances #-}

-- Есть терм
-- Есть память термов
-- Есть контекст парсинга
-- кажется это всё классы, которые могут иметь реализацию по умолчанию
-- а для каждой реализации языка потом писать свой instance

module TAPL.FullRef.Parser where

import Data.Maybe (isJust)
import Control.Monad (liftM)

import TAPL.FullRef.Types
import TAPL.FullRef.Lexer

import Text.Parsec hiding (string, parse)
import Text.Parsec.Prim (try)
import Text.Parsec.Expr
import TAPL.FullRef.Memory
import Control.Monad (guard)
import Data.Either (isLeft, isRight)

import Prelude hiding (succ, pred, lookup)
import qualified Prelude (lookup)

import Data.List (findIndex)

data LCParserContext t = LCParserContext LCNames LCMemory t
type LCParser = Parsec String (LCParserContext Term) Term
type LCTypeParser = Parsec String (LCParserContext Term) Type

-- разделение на контекст парсинга и контекс вычисления пока невозможно, так как
-- при парсинге определяется тип терма на определённых этапах, поэтому пока
-- этого делать не нужно

class ParserContext c where
    bind :: c -> String -> Binding -> c
    addName :: c -> String -> c
    isBound :: c -> String -> Bool
    pickFreshName :: c -> String -> (String, c)
    pickVar :: c -> VarName -> Maybe (String, Binding)
    nameFromContext :: c -> VarName -> Maybe String  -- можно переименовать в getName
    typeFromContext :: c -> VarName -> Maybe Binding -- можно переименовать в getType
    isVal :: c -> Bool
    isNumerical :: c -> Bool
    typeOf :: c -> Either TypeError Type
    names :: c -> LCNames
    memory :: c -> LCMemory
    withTerm :: c -> Term -> c

instance ParserContext (LCParserContext Term) where
    bind (LCParserContext n m t) x b = LCParserContext ((x,b):n) m t
    addName c x = bind c x NameBind
    isBound (LCParserContext n m _) name = isJust $ Prelude.lookup name n

    pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
    pickFreshName c name = (name, c') where c' = addName c name

    pickVar (LCParserContext [] m _) varname = Nothing
    pickVar (LCParserContext names m _) varname | length names > varname = Just $ names !! varname
    pickVar _ _ = Nothing
    nameFromContext c varname = liftM fst $ pickVar c varname
    names (LCParserContext n _ _) = n
    typeFromContext context name = liftM snd $ pickVar context name
    memory (LCParserContext _ m _) = m
    withTerm (LCParserContext ns m _) t = LCParserContext ns m t

    isVal (LCParserContext _ _ (TTrue _)) = True
    isVal (LCParserContext _ _ (TFalse _)) = True
    isVal (LCParserContext _ _ (TString _ _)) = True
    isVal (LCParserContext _ _ (TFloat _ _)) = True
    isVal (LCParserContext _ _ (TUnit _)) = True
    isVal (LCParserContext _ _ (TZero _)) = True
    isVal t | isNumerical t = True
    isVal (LCParserContext _ _ (TAbs _ _ _ _)) = True
    isVal (LCParserContext _ _ (TLoc _ _)) = True
    isVal _ = False

    isNumerical (LCParserContext _ _ (TZero _)) = True
    isNumerical (LCParserContext n s (TSucc _ x)) = isNumerical $ LCParserContext n s x
    isNumerical _ = False

    typeOf (LCParserContext _ _ (TTrue _)) = return TyBool
    typeOf (LCParserContext _ _ (TFalse _)) = return TyBool
    typeOf (LCParserContext _ _ (TString _ _)) = return TyString
    typeOf (LCParserContext _ _ (TFloat _ _)) = return TyFloat
    typeOf (LCParserContext _ _ (TUnit _)) = return TyUnit
    typeOf (LCParserContext _ _ (TZero _)) = return TyNat

    typeOf (LCParserContext n s (TSucc info t)) = do
        ty <- typeOf $ LCParserContext n s t
        case ty of
            TyNat -> return TyNat
            ty -> Left $ TypeMissmatch info $ "argument of succ is not a natural number (" ++ show ty ++ ")"

    typeOf (LCParserContext n s (TPred info t)) = do
        ty <- typeOf $ LCParserContext n s t
        case ty of
           TyNat -> return TyNat
           ty -> Left $ TypeMissmatch info $ "argument of pred is not a natural number (" ++ show ty ++ ")"

    typeOf (LCParserContext n s (TIsZero info t)) = do
        ty <- typeOf $ LCParserContext n s t
        case ty of
          TyNat -> return TyBool
          ty -> Left $ TypeMissmatch info $ "argument of zero? is not a natural number (" ++ show ty ++ ")"

    typeOf (LCParserContext n s (TIf info t1 t2 t3)) = do
        ty1 <- typeOf $ LCParserContext n s t1
        ty2 <- typeOf $ LCParserContext n s t2
        ty3 <- typeOf $ LCParserContext n s t3
        case ty1 of
             TyBool -> if ty2 == ty3
                       then return ty2
                       else Left $ TypeMissmatch info $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")"
             ty -> Left $ TypeMissmatch info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty ++ ")"

    typeOf c@(LCParserContext n s v@(TVar info varname depth)) =
        let ty = typeFromContext c varname
        in case ty of
                Just (VarBind ty') -> return ty'
                Just x -> Left $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
                Nothing -> Left $ TypeMissmatch info $ "var type error"

    typeOf (LCParserContext n s (TApp info t1 t2)) = do
        ty1 <- typeOf $ LCParserContext n s t1
        ty2 <- typeOf $ LCParserContext n s t2
        case ty1 of
             (TyArrow ty1' ty2') -> if ty2 == ty1'
                                    then return ty2'
                                    else Left $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty1 ++ " to " ++ show ty2
             x -> Left $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

    typeOf c@(LCParserContext n m (TAbs _ name ty t)) = do
        let t' = bind (LCParserContext n m t) name (VarBind ty)
        ty' <- typeOf t'
        return $ TyArrow ty ty'

    typeOf (LCParserContext n s (TRef info t)) = do
        case typeOf $ LCParserContext n s t of
             Right x -> return $ TyRef x
             x -> x

    typeOf c@(LCParserContext n s (TDeref info t)) = do
        ty <- typeOf $ LCParserContext n s t
        case ty of
             TyRef x -> return x
             x -> Left $ TypeMissmatch info $ "incorect deref not reference type (" ++ show x ++ ")"

    typeOf (LCParserContext n s (TAssign info t1 t2)) = do
        ty1 <- typeOf $ LCParserContext n s t1
        ty2 <- typeOf $ LCParserContext n s t2
        case ty1 of
             (TyRef ty2) -> return TyUnit
             _           -> Left $ TypeMissmatch info $ "invalid assignment of " ++ show ty1 ++ " to " ++ show ty2

    typeOf (LCParserContext n m (TLoc _ location)) = do
        let t = lookup m location
        ty <- typeOf $ LCParserContext n m t
        return $ TyRef ty

    typeOf c@(LCParserContext n s (TLet info v t1 t2)) = do -- вот тут ошибка, вот и всё
        ty1 <- typeOf $ LCParserContext n s t1
        let context' = bind c v (VarBind ty1)
        ty2 <- typeOf $ context' `withTerm` t2
        return ty2

--    typeOf x = error $ show x

instance Show (LCParserContext Term) where
    show (LCParserContext _ _ (TTrue _)) = "true"
    show (LCParserContext _ _ (TFalse _)) = "false"
    show (LCParserContext _ _ (TString _ x)) = show x
    show (LCParserContext _ _ (TFloat _ x)) = show x
    show (LCParserContext _ _ (TUnit _)) = "unit"
    show (LCParserContext _ _ (TZero _)) = "zero"
    show (LCParserContext n s (TSucc _ t)) = "succ " ++ show (LCParserContext n s t)
    show (LCParserContext n s (TPred _ t)) = "pred " ++ show (LCParserContext n s t)
    show (LCParserContext n s (TIsZero _ t)) = "zero? " ++ show (LCParserContext n s t)

    show (LCParserContext n s (TIf _ t1 t2 t3)) =
        "if " ++ show (LCParserContext n s t1) ++
        " then " ++ show (LCParserContext n s t2) ++
        " else " ++ show (LCParserContext n s t3)

    show c@(LCParserContext n s (TVar _ varname depth)) =
        case nameFromContext c varname of
             Just s -> s
             _ -> "[bad index in " ++ show varname ++ " in context " ++ show n  ++ "]"

    show c@(LCParserContext n s (TAbs _ name _ t)) =
        let (name', c') = pickFreshName (LCParserContext n s t) name
        in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

    show (LCParserContext n s (TApp _ t1 t2)) = show (LCParserContext n s t1) ++ " " ++ show (LCParserContext n s t2)
    show (LCParserContext n s (TRef _ t)) = "ref " ++ show (LCParserContext n s t)
    show (LCParserContext n s (TDeref _ t)) = "!" ++ show (LCParserContext n s t)
    show (LCParserContext n s (TAssign _ t1 t2)) = show (LCParserContext n s t1) ++ " := " ++ show (LCParserContext n s t2)
    show (LCParserContext n s (TLoc _ pointer)) = "<" ++ show pointer ++ ">"

    show (LCParserContext n s (TLet _ v t1 t2)) =
        "let " ++ v ++ " = " ++ show (LCParserContext n s t1) ++ " in " ++ show (LCParserContext n s t2)

parse :: String -> String -> Either ParseError (LCParserContext AST)
parse code path =
            runParser fullRefParser pureContext path code
            where pureContext = LCParserContext withoutNames emptyMemory (TUnit Nothing)
                  withoutNames = []
                  emptyMemory = LCMemory []
                  unit = TUnit Nothing

parseFile :: String -> IO (Either ParseError (LCParserContext AST))
parseFile path = do
    code <- readFile path
    return $ parse code path

fullRefParser :: Parsec String (LCParserContext Term) (LCParserContext AST)
fullRefParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of LCParserContext names memory _ -> LCParserContext names memory ast)

term :: LCParser
term = (try apply <?> "apply")
    <|> (try notApply <?> "not apply expressions")
    <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
          p <- getPosition
          whiteSpace
          return $ TApp (Just p)

notApply :: LCParser
notApply = (try assign <?> "assignment")
        <|> try condition
        <|> try let'
        <|> try value
        <|> try expression
        <|> try variable
        <|> parens notApply

assign :: LCParser
assign = chainl1 notAssign $ do
           p <- getPosition
           reservedOp ":="
           return $ TAssign (Just p)

notAssign :: LCParser
notAssign = condition
         <|> value
         <|> try expression
         <|> try variable
         <|> parens notAssign

value :: LCParser
value = boolean
     <|> string
     <|> numeric
     <|> unit
     <|> try abstraction

expression ::LCParser
expression = operator
          <|> predefinedFunction

predefinedFunction :: LCParser
predefinedFunction = ref
                  <|> isZero
                  <|> succ
                  <|> pred

operator :: LCParser
operator = deref <?> "deref"

ref :: LCParser
ref = fun "ref" TRef

deref :: LCParser
deref = do
    reservedOp "!"
    p <- getPosition
    t <- term
    return $ TDeref (Just p) t

isZero :: LCParser
isZero = fun "zero?" TIsZero

succ :: LCParser
succ = fun "succ" TSucc

pred :: LCParser
pred = fun "pred" TPred

fun :: String -> (Info -> Term -> Term) -> LCParser
fun name tm = do
    reserved name
    p <- getPosition
    t <- term
    return $ tm (Just p) t

boolean :: LCParser
boolean = true <|> false
    where true = constant "true" TTrue
          false = constant "false" TFalse

string :: LCParser
string = do
    p <- getPosition
    t <- try stringLiteral
    return $ TString (Just p) t

numeric :: LCParser
numeric = zero
       <|> (float <?> "float")

unit :: LCParser
unit = constant "unit" TUnit

zero :: LCParser
zero = constant "zero" TZero

float :: LCParser
float = do
    p <- getPosition
    n <- floatNum
    return $ TFloat (Just p) n

constant :: String -> (Info -> Term) -> LCParser
constant name t = do
    p <- getPosition
    reserved name
    return $ t (Just p)

condition :: LCParser
condition = do
    p <- getPosition
    reserved "if"
    t1 <- term
    reserved "then"
    t2 <- term
    reserved "else"
    t3 <- term
    return $ TIf (Just p) t1 t2 t3

let' :: LCParser
let' = do
    reserved "let"
    p <- getPosition
    v <- identifier
    reserved "="
    t1 <- term
    reserved "in"
    context <- getState
    let ty = typeOf $ context `withTerm` t1
    guard (isRight ty) <?> "type missmatch"
    modifyState $ \c -> bind c v $ VarBind (fromRight ty)
    t2 <- term
    return $ TLet (Just p) v t1 t2

abstraction :: LCParser
abstraction = do
    p <- getPosition
    reserved "lambda"
    varName <- identifier
    varType <- termType
    dot
    optional space
    context <- getState
    modifyState $ (\_ -> bind context varName $ (VarBind varType))
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
        Nothing -> error $ "variable " ++ name ++ " has't been bound in context " ++ " " ++ (show p) -- todo ?
        Just n -> return $ TVar (Just p) n (length $ ns)

termType :: LCTypeParser
termType = do
    colon
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
notArrowAnnotation = booleanAnnotation
                  <|> stringAnnotation
                  <|> natAnnotation
                  <|> floatAnnotation
                  <|> unitAnnotation
                  <|> refAnnotation

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty

booleanAnnotation :: LCTypeParser
booleanAnnotation = primitiveType "Bool" TyBool

stringAnnotation :: LCTypeParser
stringAnnotation = primitiveType "String" TyString

natAnnotation :: LCTypeParser
natAnnotation = primitiveType "Nat" TyNat

floatAnnotation :: LCTypeParser
floatAnnotation = primitiveType "Float" TyFloat

unitAnnotation :: LCTypeParser
unitAnnotation = primitiveType "Unit" TyUnit

refAnnotation :: LCTypeParser
refAnnotation = do
    reserved "Ref"
    ty <- typeAnnotation
    return $ TyRef ty

fromRight (Right x) = x
fromRight (Left x) = undefined
