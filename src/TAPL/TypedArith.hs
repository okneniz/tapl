{-# LANGUAGE FlexibleContexts #-}

module TAPL.TypedArith (eval) where

import Prelude hiding (pred, succ)

import Control.Monad
import Control.Applicative hiding ((<|>))
import Control.Exception (Exception)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim (try, ParsecT, Stream)

data Term = TTrue
          | TFalse
          | TZero
          | TIf Term Term Term
          | TSucc Term
          | TPred Term
          | TIsZero Term
          | TBraces Term

data Type = TyNat | TyBool deriving (Eq)

instance Show Term where
    show TTrue = "true"
    show TFalse = "false"
    show TZero = "0"
    show (TIf x y z) = "if " ++ (show x) ++ " then " ++ (show y) ++ " else " ++ (show z)
    show (TSucc x) = "succ " ++ (show x)
    show (TPred x) = "pred " ++ (show x)
    show (TIsZero x) = "isZero " ++ (show x)
    show (TBraces x) = "(" ++ (show x) ++ ")"

instance Show Type where
    show TyNat = "Nat"
    show TyBool = "Bool"

typeof :: Term -> Either String Type
typeof TTrue = return TyBool
typeof TFalse = return TyBool
typeof TZero = return TyNat
typeof (TSucc t) | typeof t == (Right TyNat) = return TyNat
typeof (TSucc t) = Left "argument of succ is not a number"
typeof (TPred t) | typeof t == (Right TyNat) = return TyNat
typeof (TPred t) = Left "argument of pred is not a number"
typeof (TIsZero t) | typeof t == (Right TyNat) = return TyBool
typeof (TIsZero t) = Left "argument of isZero is not a number"
typeof (TBraces t) = typeof t
typeof (TIf t1 t2 t3) | typeof t1 == Right TyBool = if t2' == t3'
                                                    then t2'
                                                    else Left "arms of conditional have different types"
                                              where t2' = typeof t2
                                                    t3' = typeof t3
typeof (TIf _ _ _) = Left "guard of conditional not a boolean"

isNumerical :: Term -> Bool
isNumerical TZero = True
isNumerical (TSucc x) = isNumerical x
isNumerical (TBraces x) = isNumerical x
isNumerical _ = False

isVal :: Term -> Bool
isVal TTrue = True
isVal TFalse = True
isVal (TBraces x) = isVal x
isVal x | isNumerical x = True
isVal _ = False

eval :: String -> String
eval x = case term' of
              Right x -> case typeof x of
                              Right t' -> (show $ f x) ++ ":" ++ (show t')
                              Left x -> x
              Left x -> show x
   where term' = parse (term <* eof) "arith" x
         f t = case normalize t of
                    Just t' -> f t'
                    Nothing -> t

normalize :: Term -> Maybe Term
normalize (TBraces x) = return x
normalize TTrue = Nothing
normalize TFalse = Nothing
normalize TZero = Nothing
normalize (TIf TTrue x _) = return x
normalize (TIf TFalse _ x) = return x
normalize (TIf x y z) = liftM3 TIf (normalize x) (return y) (return z)
normalize (TSucc x) = liftM TSucc (normalize x)
normalize (TPred TZero) = return TZero
normalize (TPred (TSucc x)) | isNumerical x = return x
normalize (TPred x) = liftM TPred (normalize x)
normalize (TIsZero TZero) = return TTrue
normalize (TIsZero (TSucc x)) | isNumerical x = return TFalse
normalize (TIsZero x) = liftM TIsZero (normalize x)

term :: Parser Term
term = (try condition) <|>
       (try isZero)    <|>
       (try braces)    <|>
       (try boolean)   <|>
       numeric

boolean :: Parser Term
boolean = true <|> false

numeric :: Parser Term
numeric = zero <|> succ <|> pred

true :: Parser Term
true = do
    padded "true"
    return TTrue

false :: Parser Term
false = do
    padded "false"
    return TFalse

padded :: Stream s m Char => String -> ParsecT s u m String
padded x = spaces *> string x <* spaces

succ :: Parser Term
succ = do
    padded "succ"
    t <- term
    return $ TSucc t

pred :: Parser Term
pred = do
    padded "pred"
    t <- term
    return $ TPred t

zero :: Parser Term
zero = do
    padded "0"
    return TZero

condition :: Parser Term
condition = do
    padded "if"
    x <- term
    padded "then"
    y <- term
    padded "else"
    z <- term
    return $ TIf x y z

isZero :: Parser Term
isZero = do
    padded "isZero"
    t <- term
    return $ TIsZero t

braces :: Parser Term
braces = do
    t <- padded "(" *> term <* padded ")"
    return $ TBraces t
