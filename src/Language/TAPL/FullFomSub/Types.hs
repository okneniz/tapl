module Language.TAPL.FullFomSub.Types where

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

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term Term
          | TLet SourcePos String Term Term
          | TFix SourcePos Term
          | TString SourcePos String
          | TUnit SourcePos
          | TAscribe SourcePos Term Type
          | TFloat SourcePos Double
          | TTimesFloat SourcePos Term Term
          | TTAbs SourcePos String Type Term
          | TTApp SourcePos Term Type
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TPack SourcePos Type Term Type
          | TUnpack SourcePos String String Term Term
          | TKeyword SourcePos String
          deriving (Show)

type AST = [Term]
type Location = Int

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TFloat _ _) = True
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
          | TyTop
          | TyArrow Type Type
          | TyBool
          | TyRecord (Map String Type)
          | TyString
          | TyUnit
          | TyFloat
          | TyAll String Type Type
          | TyNat
          | TySome String Type Type
          | TyAbs String Kind Type
          | TyApp Type Type
          | TyKeyword
          deriving (Show, Eq)

data Binding = NameBind
             | TypeVarBind Type
             | VarBind Type
             | TypeAddBind Type (Maybe Kind)
             deriving (Show)
