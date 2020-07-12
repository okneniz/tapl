module Language.TAPL.Equirec.Types where

import Text.Parsec (SourcePos)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term

type VarName = Int
type Depth = Int
type AST = [Term]

data Binding = NameBind | VarBind Type

data Type = TyArrow Type Type
          | TyID String
          | TyVar VarName Depth
          | TyRec String Type

instance Eq Type where
    (TyID x) == (TyID y) = x == y
    (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)
    _ == _ = False

(<:) :: Type -> Type -> Bool
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
x <: y | x == y = True
_ <: _ = False

