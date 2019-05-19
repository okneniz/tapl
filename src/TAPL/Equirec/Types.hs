{-# LANGUAGE FlexibleInstances #-}

module TAPL.Equirec.Types where

import Prelude
import qualified Prelude (lookup)

import Text.Parsec (SourcePos)
import Text.Parsec.Error (ParseError(..))
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)
import Data.Function (on)
import Data.List (intercalate, all, sortBy)

data Term = TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term

type VarName = Int
type Depth = Int
type Info = Maybe SourcePos
type AST = [Term]

data Binding = NameBind | VarBind Type
type LCNames = [(String,Binding)]
data LCMemory = LCMemory [Term]

data Type = TyArrow Type Type
          | TyID String
          | TyVar VarName Depth
          | TyRec String Type

data EvaluationError = ParsecError ParseError
                     | TypeError String
                     deriving (Eq)

instance Show EvaluationError where
    show (ParsecError e) = show e
    show (TypeError s) = show s

data TypeError = TypeMissmatch Info String

instance Show TypeError where
    show (TypeMissmatch (Just info) message) =
        let line = sourceLine info
            column = sourceColumn info
            name = sourceName info
         in case name of
                 "" -> message ++ " in " ++ show line ++ ":" ++ show column
                 _ -> message ++ " in " ++ name ++ show line ++ ":" ++ show column

instance Eq Type where
  (TyID x) == (TyID y) = x == y
  (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)

(<:) :: Type -> Type -> Bool
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
x <: y | x == y = True
x <: y = False
