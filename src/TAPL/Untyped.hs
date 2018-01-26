module TAPL.Untyped where

import Prelude hiding (abs)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (elemIndex, intersperse)

type VarName = Int
type Depth = Int

data Term = TVar VarName Depth
          | TAbs String Term
          | TApp Term Term
          deriving (Show, Eq)

type Context = [String]

parse' :: String -> Either ParseError (Term, Context)
parse' = runParser expression [] "untyped λ-calculus"

expression :: Parsec String Context (Term, Context)
expression = do
    t <- term <* spaces <* eof
    c <- getState
    return (t, c)

term :: Parsec String Context Term
term = chainl1 (abs <|> var <|> parens term) (return TApp)

var :: Parsec String Context Term
var = do
    name <- identifier
    context <- getState
    case elemIndex name context of
        Nothing -> fail $ "variable " ++ name ++ " has't been bound in context"
        Just n -> return $ TVar n (length context)

identifier :: Parsec String Context String
identifier = do
    v <- letter
    s <- optionMaybe $ many $ char '\''
    case s of
        Just s' -> return $ v:s'
        Nothing -> return [v]

abs :: Parsec String Context Term
abs = do
    char 'λ'
    v <- identifier
    modifyState (v:)
    char '.'
    t <- term
    modifyState tail
    return $ TAbs v t

parens :: Parsec String Context Term -> Parsec String Context Term
parens = between (char '(') (char ')')

eval :: String -> String
eval x = case parse' x of
              Right (t,c) -> display c (f t)
              Left x -> show x
   where  f t = case eval' t of
                     Just t' -> t'
                     Nothing -> t

eval' :: Term -> Maybe Term
eval' (TApp (TAbs name t) v) | isVal v = return $ substitutionTop v t
eval' (TApp v t) | isVal v = liftM2 TApp (return v) (eval' t)
eval' (TApp t1 t2) = liftM2 TApp (eval' t1) (return t2)
eval' _ = Nothing

isVal :: Term -> Bool
isVal (TAbs _ _) = True
isVal _ = False

tmmap :: (Int -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar name depth) = onvar c name depth
                  walk c (TAbs x t2) = TAbs x (walk (c+1) t2)
                  walk c (TApp t1 t2) = TApp (walk c t1) (walk c t2)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = tmmap f c t
                 where f c name depth | name >= c = TVar (name + d) (depth + d)
                       f c name depth = TVar name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap f 0 t
               where f c name depth | name == j + c = shift c s
                     f c name depth = TVar name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)

pickFreshName :: String -> Context -> (String, Context)
pickFreshName x ctx = (x', x':ctx)
                where x' = freshName x ctx
                      freshName x [] = x
                      freshName x ctx@(b:bs) | x == b    = freshName (x ++ "'") ctx
                                             | otherwise = freshName x bs

symbolyze :: Context -> VarName -> String
symbolyze context name = context !! name

display :: Context -> Term -> String
display context (TAbs name x) = "(λ" ++ name' ++ "." ++ (display context' x) ++ ")" where (name', context') = pickFreshName name context
display context (TApp x y) = (display context x) ++ (display context y)
display context (TVar name depth) = if (length context) == depth then symbolyze context name else "[bad index]"
