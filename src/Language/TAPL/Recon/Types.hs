module Language.TAPL.Recon.Types where

import Text.Parsec (SourcePos)
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
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

type VarIndex = Int
type AST = [Term]
type Constraint = (Type, Type)

data TypeError = TypeMissmatch SourcePos String
               | CircularConstrains [Constraint]
               | UnresolvedConstraints [Constraint]

instance Show TypeError where
    show (TypeMissmatch pos message) = message <> " in " <> show pos
    show (CircularConstrains cs) = "Circular constraints " <> show cs
    show (UnresolvedConstraints cs) = "Unresolved constraints " <> show cs
