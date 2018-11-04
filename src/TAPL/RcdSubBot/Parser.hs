{-# LANGUAGE FlexibleInstances #-}

module TAPL.RcdSubBot.Parser where

import Data.Maybe (isJust)
import Control.Monad (liftM)

import TAPL.RcdSubBot.Types
import TAPL.RcdSubBot.Lexer

import Text.Parsec hiding (string, parse)
import Text.Parsec.Prim (try)
import Text.Parsec.Expr
import Control.Monad (guard)
import Data.Either (isLeft, isRight)

import Prelude hiding (succ, pred, lookup)
import qualified Prelude (lookup)

import Data.List (findIndex, all, intercalate)

data LCParserContext t = LCParserContext LCNames (Maybe t)
type LCParser = Parsec String (LCParserContext Term) Term
type LCTypeParser = Parsec String (LCParserContext Term) Type

class ParserContext c where
    bind :: c -> String -> Binding -> c
    addName :: c -> String -> c
    isBound :: c -> String -> Bool
    pickFreshName :: c -> String -> (String, c)
    pickVar :: c -> VarName -> Maybe (String, Binding)
    nameFromContext :: c -> VarName -> Maybe String
    typeFromContext :: c -> VarName -> Maybe Binding
    isVal :: c -> Bool
    typeOf :: c -> Either TypeError Type
    names :: c -> LCNames
    withTerm :: c -> Term -> c

instance ParserContext (LCParserContext Term) where
    bind (LCParserContext n t) x b = LCParserContext ((x,b):n) t
    addName c x = bind c x NameBind
    isBound (LCParserContext n _) name = isJust $ Prelude.lookup name n

    pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
    pickFreshName c name = (name, c') where c' = addName c name

    pickVar (LCParserContext [] _) varname = Nothing
    pickVar (LCParserContext names _) varname | length names > varname = Just $ names !! varname
    pickVar _ _ = Nothing
    nameFromContext c varname = liftM fst $ pickVar c varname
    names (LCParserContext n _) = n
    typeFromContext context name = liftM snd $ pickVar context name
    withTerm (LCParserContext ns _) t = LCParserContext ns $ Just t

    isVal (LCParserContext _ (Just (TAbs _ _ _ _))) = True
    isVal c@(LCParserContext _ (Just (TRecord _ fs))) = all (\(_, t) -> isVal $ c `withTerm` t) fs
    isVal _ = False

    typeOf c@(LCParserContext n v@(Just (TRecord info fields))) = do
        let f t = typeOf $ c `withTerm` t
        let tyFields = (\(k, v) -> (k, f v)) <$> fields
        let check = all (\(k, v) -> isRight v) tyFields
        let tys = fmap (\(k, (Right t)) -> (k,t)) tyFields
        let err = head $ fmap snd $ filter (\(_, v) -> isLeft v) tyFields
        if check
        then return $ TyRecord tys
        else err

    typeOf c@(LCParserContext n v@(Just (TVar info varname depth))) =
        let ty = typeFromContext c varname
        in case ty of
                Just (VarBind ty') -> return ty'
                Just x -> Left $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
                Nothing -> Left $ TypeMissmatch info $ "var type error"

    typeOf c@(LCParserContext n (Just (TAbs _ name ty t))) = do
        let t' = bind (LCParserContext n $ Just t) name (VarBind ty)
        ty' <- typeOf t'
        return $ TyArrow ty ty'

    typeOf (LCParserContext n (Just (TApp info t1 t2))) = do
        ty1 <- typeOf $ LCParserContext n $ Just t1
        ty2 <- typeOf $ LCParserContext n $ Just t2
        case ty1 of
             (TyArrow ty1' ty2') -> if isSubType ty2 ty1'
                                    then return ty2'
                                    else Left $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty1 ++ " to " ++ show ty2
             TyBot -> return TyBot
             x -> Left $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

    typeOf c@(LCParserContext n (Just (TProj info k t))) = do
        case typeOf $ c `withTerm` t of
             (Right (TyRecord fs)) -> case (Prelude.lookup k fs) of
                                           (Just v) -> return v
                                           _ -> Left $ TypeMissmatch info $ "Invalid record type"
             (Right TyBot) -> return TyBot
             _ -> Left $ TypeMissmatch info $ "Expected record type"

isSubType _ TyTop = True
isSubType TyBot _ = True
isSubType (TyArrow tys1 tys2) (TyArrow tyt1 tyt2) = (isSubType tyt1 tys1) && (isSubType tys2 tyt2)
isSubType (TyRecord fS) (TyRecord fT) = all (\(k,tTi) -> case Prelude.lookup k fS of Just tSi -> isSubType tSi tTi; _ -> False) fT
isSubType _ _ = False

instance Show (LCParserContext Term) where
    show c@(LCParserContext n (Just (TVar _ varname depth))) =
        case nameFromContext c varname of
             Just s -> s
             _ -> "[bad index in " ++ show varname ++ " in context " ++ show n  ++ "]"

    show c@(LCParserContext n (Just (TAbs _ name _ t))) =
        let (name', c') = pickFreshName (LCParserContext n $ Just t) name
        in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

    show (LCParserContext n (Just (TApp _ t1 t2))) = show (LCParserContext n $ Just t1) ++ " " ++ show (LCParserContext n $ Just t2)

    show c@(LCParserContext n (Just (TProj _ k t))) = (show $ c `withTerm` t) ++ "." ++ k

    show c@(LCParserContext n (Just (TRecord _ fs))) = "{" ++ (list fs) ++ "}"
        where pair (k,v) = k ++ "=" ++ (show $ c `withTerm` v)
              list l = intercalate "," $ pair <$> l

parse :: String -> String -> Either ParseError (LCParserContext AST)
parse code path = runParser fullErrorParser pureContext path code
          where pureContext = LCParserContext withoutNames Nothing
                withoutNames = []

parseFile :: String -> IO (Either ParseError (LCParserContext AST))
parseFile path = do
    code <- readFile path
    return $ parse code path

fullErrorParser :: Parsec String (LCParserContext Term) (LCParserContext AST)
fullErrorParser = do
    ast <- term `sepEndBy` semi
    eof
    context <- getState
    return (case context of LCParserContext names _ -> LCParserContext names $ Just ast)

term :: LCParser
term = (try apply <?> "apply")
    <|> (try notApply)
    <|> parens term

apply :: LCParser
apply = chainl1 notApply $ do
          p <- getPosition
          whiteSpace
          return $ TApp (Just p)

notApply :: LCParser
notApply =  (try abstraction <?> "abstraction")
        <|> (try lookup' <?> "lookup")
        <|> ((try $ braces record) <?> "record")
        <|> (try variable <?> "variable")
        <|> parens notApply

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

record :: LCParser
record = do
    ts <- (keyValue "=") `sepBy` comma
    p <- getPosition
    return $ TRecord (Just p) ts

keyValue :: String -> Parsec String (LCParserContext Term) (String, Term)
keyValue s = do
    k <- identifier
    reserved s
    v <- term
    return (k, v)

lookup' :: LCParser
lookup' = do
    t <- variable <|> record
    dot
    k <- identifier
    context <- getState
    p <- getPosition
    return $ TProj (Just p) k t

termType :: LCTypeParser
termType = do
    colon
    ty <- typeAnnotation
    return ty

typeAnnotation :: LCTypeParser
typeAnnotation = try notArrowAnnotation
             <|> (try arrowAnnotation <?> "arrow anotation")
             <|> parens typeAnnotation

arrowAnnotation :: LCTypeParser
arrowAnnotation = chainr1 (notArrowAnnotation <|> parens arrowAnnotation) $ do
                    reserved "->"
                    return TyArrow

notArrowAnnotation :: LCTypeParser
notArrowAnnotation = ((try $ braces recordAnnotation) <?> "record anotation")
                 <|> (try topAnnotation <?> "top annotation")
                 <|> (try botAnnotation <?> "bot annotation")

recordAnnotation :: LCTypeParser
recordAnnotation = do
    tys <- keyValue2 `sepBy` comma
    return $ TyRecord tys

keyValue2 :: Parsec String (LCParserContext Term) (String, Type)
keyValue2 = do
    k <- identifier
    colon
    v <- typeAnnotation
    return (k, v)

primitiveType :: String -> Type -> LCTypeParser
primitiveType name ty = do
    reserved name
    return ty

topAnnotation :: LCTypeParser
topAnnotation = primitiveType "Top" TyTop

botAnnotation :: LCTypeParser
botAnnotation = primitiveType "Bot" TyBot
