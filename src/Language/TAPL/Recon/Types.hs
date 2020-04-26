module Language.TAPL.Recon.Types where

data Command = Eval [Term]
             | Bind Info String Binding
             deriving (Show)

data Term = TTrue Info
          | TFalse Info
          | TIf Info Term Term Term
          | TZero Info
          | TSucc Info Term
          | TPred Info Term
          | TIsZero Info Term
          | TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          deriving (Show)

data Type = TyBool
          | TyArrow Type Type
          | TyNat
          | TyID String
          | TyVar VarName Depth
          deriving (Show)

data Binding = NameBind
             | VarBind Type
             deriving (Show)

data Info = Info { row :: Int, column :: Int } deriving (Show, Eq)

type VarName = Int
type VarIndex = Int
type Depth = Int
type AST = [Term]
type Constraint = (Type, Type)

data TypeError = TypeMissmatch Info String
               | CircularConstrains [Constraint]
               | UnresolvedConstraints [Constraint]

instance Show TypeError where
    show (TypeMissmatch info message) =
        message ++ " in " ++ (show $ row info) ++ ":" ++ (show $ column info)

    show (CircularConstrains cs) = "Circular constraints " ++ show cs
    show (UnresolvedConstraints cs) = "Unresolved constraints " ++ show cs
