module Language.TAPL.Untyped.Types where

data Term = TVar Info VarName Depth
          | TAbs Info String Term
          | TApp Info Term Term
          deriving (Show)

type VarName = Int
type Depth = Int
data Info = Info { row :: Int, column :: Int } deriving (Eq)
type AST = [Term]

data Binding = NameBind deriving (Show)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)
