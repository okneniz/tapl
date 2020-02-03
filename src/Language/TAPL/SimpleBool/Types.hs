module Language.TAPL.SimpleBool.Types where

data Term = TTrue Info
          | TFalse Info
          | TIf Info Term Term Term
          | TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          deriving (Show)

data Type = TyBool
          | TyArrow Type Type
          deriving (Eq)

type VarName = Int
type Depth = Int
data Info = Info { row :: Int, column :: Int } deriving (Eq)
type Location = Int
type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

instance Show Type where
    show (TyArrow t1 t2) = "("++ show t1 ++ " -> " ++ show t2 ++ ")"
    show TyBool = "Bool"