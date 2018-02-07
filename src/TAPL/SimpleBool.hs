{-# LANGUAGE FlexibleContexts #-}

module TAPL.SimpleBool (eval) where

import Prelude hiding (abs)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intersperse)

type VarName = Int
type Depth = Int

data Info = Info { row :: Int, column :: Int } deriving (Eq)

data Term = TTrue Info
          | TFalse Info
          | TIf Info Term Term Term
          | TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          deriving (Show, Eq)

data Type = TyBool | TyArrow Type Type deriving (Eq)

data Binding = NameBind | VarBind Type deriving (Show)

type Context = [(String,Binding)]

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

instance Show Type where
    show (TyArrow t1 t2) = "(" ++ (show t1) ++ "->" ++ (show t1) ++ ")"
    show TyBool = "Bool"

-- Context

bind :: String -> Binding -> Context -> Context
bind x bind ctx = (x,bind):ctx

addName :: String -> Context -> Context
addName x ctx = bind x NameBind ctx

isNameBound :: String -> Context -> Bool
isNameBound name [] = False
isNameBound name ((x,_):xs) | x == name = True
isNameBound name ((_,_):xs) = isNameBound name xs

pickFreshName :: String -> Context -> (String, Context)
pickFreshName name context | isNameBound name context = pickFreshName (name ++ "'") context
pickFreshName name context = (name, (addName name context))

pickVar :: Context -> VarName -> Maybe (String, Binding)
pickVar [] name = Nothing
pickVar context name | length context > name = Just $ context !! name
pickVar _ _ = Nothing

nameFromContext :: Context -> VarName -> Maybe String
nameFromContext context name = liftM fst $ pickVar context name

typeFromContext :: Context -> VarName -> Maybe Binding
typeFromContext context name = liftM snd $ pickVar context name

display :: Context -> Term -> String
display _ (TTrue _) = "true"
display _ (TFalse _) = "false"
display c (TIf info t1 t2 t3) = "if " ++ display c t1 ++ " then " ++ display c t2 ++ " else " ++ display c t3
display c (TVar _ name depth) =
    let s = nameFromContext c name
    in case s of
            Just s -> s
            _ -> "[bad index]"

display context (TAbs info name ty x) =
    let (name', context') = pickFreshName name context
    in "(λ" ++ name' ++ "." ++ (display context' x) ++ ")"

display context (TApp info x y) = (display context x) ++ (display context y) ++ ":" ++ (show $ typeof context $ TApp info x y)

-- Parsing

parse' :: String -> Either ParseError (Context, Term)
parse' = runParser expression [] "simple typed λ-calculus"

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

parens :: Parsec String Context a -> Parsec String Context a
parens = between (char '(') (char ')')

padded :: Stream s m Char => String -> ParsecT s u m String
padded x = spaces *> string x <* spaces

expression :: Parsec String Context (Context, Term)
expression = do
    t <- term <* spaces <* eof
    c <- getState
    return (c, t)

term :: Parsec String Context Term
term = chainl1 (try boolean <|> try condition <|> try abs <|> try var <|> parens term) $ do
           pos <- getPosition
           return $ TApp (infoFrom pos)

abs :: Parsec String Context Term
abs = do
    char 'λ'
    v <- identifier
    vt <- termType
    modifyState $ bind v $ VarBind vt
    char '.'
    t <- term
    modifyState tail
    pos <- getPosition
    return $ TAbs (infoFrom pos) v vt t

var :: Parsec String Context Term
var = do
    name <- identifier
    context <- getState
    pos <- getPosition
    case findIndex ((== name) . fst) context of
        Nothing -> fail $ "variable " ++ name ++ " has't been bound in context " ++ (show context)
        Just n -> return $ TVar (infoFrom pos) n (length context)

boolean :: Parsec String Context Term
boolean = try true <|> false

termType :: Parsec String Context Type
termType = do
    char ':'
    typeAnnotation

typeAnnotation :: Parsec String Context Type
typeAnnotation = try arrowAnnotation <|> booleanAnnotation

booleanAnnotation :: Parsec String Context Type
booleanAnnotation = do
    string "Bool"
    return $ TyBool

arrowAnnotation :: Parsec String Context Type
arrowAnnotation = do
    t1 <- booleanAnnotation
    padded "->"
    t2 <- booleanAnnotation
    return $ TyArrow t1 t2

true :: Parsec String Context Term
true = do
    padded "true"
    pos <- getPosition
    return $ TTrue (infoFrom pos)

false :: Parsec String Context Term
false = do
    padded "false"
    pos <- getPosition
    return $ TFalse (infoFrom pos)

condition :: Parsec String Context Term
condition = do
    padded "if"
    x <- term
    padded "then"
    y <- term
    padded "else"
    z <- term
    pos <- getPosition
    return $ TIf (infoFrom pos) x y z

identifier :: Parsec String Context String
identifier = do
    v <- letter
    s <- optionMaybe $ many $ char '\''
    case s of
        Just s' -> return $ v:s'
        Nothing -> return [v]

-- Evaluation

eval :: String -> String
eval code = case parse' code of
                 Right (context, term') ->
                    let result = f context term' in
                    case typeof context result of
                         Right t' -> (display context result) ++ ":" ++ (show t')
                         Left x -> x
                 Left x -> show x
   where f c t = case eval' c t of
                      Just t' -> f c t'
                      Nothing -> t

eval' :: Context -> Term -> Maybe Term
eval' context (TIf _ (TTrue _) t _ ) = return t
eval' context (TIf _ (TFalse _) _ t) = return t
eval' context (TIf info t1 t2 t3) = liftM (\t1' -> TIf info t1' t2 t3) (eval' context t1)

eval' context (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t

eval' context (TApp info t1 t2) | isVal t1   = liftM2 (TApp info) (return t1) (eval' context t2)
                                | otherwise  = liftM2 (TApp info) (eval' context t1) (return t2)
eval' _ _ = Nothing

typeof :: Context -> Term -> Either String Type
typeof context (TTrue _) = return TyBool
typeof context (TFalse _)= return TyBool

typeof context (TIf info t1 t2 t3) | typeof context t1 == Right TyBool =
    let t2' = typeof context t2
        t3' = typeof context t3
    in if t2' == t3'
       then t2'
       else Left $ "arms of conditional have different types " ++ (show info)

typeof context (TIf info _ _ _) = Left $ "guard of conditional not a boolean " ++ (show info)

typeof context (TVar info name depth) =
    let ty = typeFromContext context name
    in case ty of
            Just (VarBind ty') -> Right ty'
            _ -> Left "var type error"

typeof context (TApp info t1 t2) =
    let ty1 = typeof context t1
        ty2 = typeof context t2
    in case ty1 of
        Right (TyArrow ty1' ty2') -> if ty2 == (Right ty1')
                                     then Right ty2'
                                     else Left $ "type mismatch in " ++ show info
        Left x -> Left x

typeof context (TAbs info name ty t) =
    let context' = bind name (VarBind ty) context
        ty' = typeof context' t
    in case ty' of
            Right ty'' -> Right $ TyArrow ty ty''
            Left x -> Left x

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk c (TTrue info) = TTrue info
                  walk c (TFalse info) = TFalse info

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = tmmap onvar c t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar c info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c info name depth | name == j + c = shift c s
                     onvar c info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
