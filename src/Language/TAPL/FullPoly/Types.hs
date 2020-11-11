module Language.TAPL.FullPoly.Types where

import Text.Parsec (SourcePos)

import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             | SomeBind SourcePos String String Term
             deriving (Show)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TLet SourcePos String Term Term
          | TFix SourcePos Term
          | TString SourcePos String
          | TUnit SourcePos
          | TAscribe SourcePos Term Type
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term String
          | TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TFloat SourcePos Double
          | TTimesFloat SourcePos Term Term
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TPack SourcePos Type Term Type
          | TUnpack SourcePos String String Term Term
          | TTAbs SourcePos String Term
          | TTApp SourcePos Term Type
          deriving (Show)

type AST = [Term]

isVal :: Term -> Bool
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TFloat _ _) = True
isVal x | isNumerical x = True
isVal (TAbs _ _ _ _) = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TPack _ _ v1 _) = isVal v1
isVal (TTAbs _ _ _) = True
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

data Type = TyVar VarName Depth
          | TyID String
          | TyArrow Type Type
          | TyString
          | TyUnit
          | TyRecord (Map String Type)
          | TyBool
          | TyFloat
          | TyNat
          | TySome String Type
          | TyAll String Type
          deriving (Show, Eq)

data Binding = NameBind
             | VarBind Type
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)
