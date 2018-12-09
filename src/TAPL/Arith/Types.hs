module TAPL.Arith.Types where

data Term = TTrue
          | TFalse
          | TZero
          | TIf Term Term Term
          | TSucc Term
          | TPred Term
          | TIsZero Term

instance Show Term where
    show TTrue = "true"
    show TFalse = "false"
    show TZero = "zero"
    show (TIf x y z) = "if " ++ (show x) ++ " then " ++ (show y) ++ " else " ++ (show z)
    show (TSucc x) = "succ " ++ (show x)
    show (TPred x) = "pred " ++ (show x)
    show (TIsZero x) = "zero? " ++ (show x)

isNumerical :: Term -> Bool
isNumerical TZero = True
isNumerical (TSucc x) = isNumerical x
isNumerical _ = False

isVal :: Term -> Bool
isVal TTrue = True
isVal TFalse = True
isVal x | isNumerical x = True
isVal _ = False
