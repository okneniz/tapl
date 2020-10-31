module Language.TAPL.FullOmega.Types where

import Text.Parsec (SourcePos)

import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             | SomeBind SourcePos String String Term
             deriving (Show)

data Kind = Star | Arrow Kind Kind deriving (Show, Eq)

data Term = TAscribe SourcePos Term Type
          | TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term Term
          | TString SourcePos String
          | TUnit SourcePos
          | TLoc SourcePos Location
          | TRef SourcePos Term
          | TDeref SourcePos Term
          | TAssign SourcePos Term Term
          | TFloat SourcePos Double
          | TTimesFloat SourcePos Term Term
          | TLet SourcePos String Term Term
          | TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TFix SourcePos Term
          | TTAbs SourcePos String Kind Term
          | TTApp SourcePos Term Type
          | TPack SourcePos Type Term Type
          | TUnpack SourcePos String String Term Term
          | TKeyword SourcePos String
          | TInt SourcePos Integer
          deriving (Show)

type AST = [Term]
type Location = Int

isVal :: Term -> Bool
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TLoc _ _) = True
isVal (TFloat _ _) = True
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal x | isNumerical x = True
isVal (TAbs _ _ _ _) = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TPack _ _ v _) | isVal v = True
isVal (TTAbs _ _ _ _) = True
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

data Type = TyVar VarName Depth
          | TyID String
          | TyArrow Type Type
          | TyRecord (Map String Type)
          | TyRef Type
          | TyString
          | TyUnit
          | TyBool
          | TyFloat
          | TyAll String Kind Type
          | TyNat
          | TySome String Kind Type
          | TyAbs String Kind Type
          | TyApp Type Type
          | TyKeyword
          | TyInt
          deriving (Show, Eq)

data Binding = NameBind
             | TypeVarBind Kind
             | VarBind Type
             | TypeAddBind Type (Maybe Kind)
             deriving (Show)
