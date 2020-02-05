module Language.TAPL.Bot.Types where

data Term = TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          deriving (Show)

type VarName = Int
type Depth = Int
data Info = Info { row :: Int, column :: Int } deriving (Show)
type AST = [Term]

data Binding = NameBind
             | VarBind Type
             deriving (Show)

data Type = TyArrow Type Type
          | TyTop
          | TyBot
          deriving (Show)

instance Eq Type where
  TyTop == TyTop = True
  TyBot == TyBot = True
  (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)
  _ == _ = False