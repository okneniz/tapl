module Language.TAPL.FOmega.Types where

import Text.Parsec (SourcePos)

import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Kind = Star | Arrow Kind Kind deriving (Show, Eq)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TTAbs SourcePos String Kind Term
          | TTApp SourcePos Term Type
          deriving (Show)

type AST = [Term]
type Location = Int

isVal :: Term -> Bool
isVal (TAbs _ _ _ _) = True
isVal (TTAbs _ _ _ _) = True
isVal _ = False

data Type = TyVar VarName Depth
          | TyArrow Type Type
          | TyAll String Kind Type
          | TyAbs String Kind Type
          | TyApp Type Type
          deriving (Show, Eq)

data Binding = NameBind
             | TypeVarBind Kind
             | VarBind Type
             deriving (Show)
