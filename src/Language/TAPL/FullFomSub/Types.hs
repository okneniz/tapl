module Language.TAPL.FullFomSub.Types where

import Text.Parsec (SourcePos)

import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Language.TAPL.Common.Context

--type command =
--  | Eval of info * term
--  | Bind of info * string * binding
--  | SomeBind of info * string * string * term

data Command = Eval [Term]
             | Bind SourcePos String Binding
             | SomeBind SourcePos String String Term
             deriving (Show)

--type kind =
--    KnStar
--  | KnArr of kind * kind

data Kind = Star | Arrow Kind Kind deriving (Show, Eq)

--type term =
--    TmVar of info * int * int
--  | TmAbs of info * string * ty * term
--  | TmApp of info * term * term
--  | TmTrue of info
--  | TmFalse of info
--  | TmIf of info * term * term * term
--  | TmRecord of info * (string * term) list
--  | TmProj of info * term * string
--  | TmLet of info * string * term * term
--  | TmFix of info * term
--  | TmString of info * string
--  | TmUnit of info
--  | TmAscribe of info * term * ty
--  | TmFloat of info * float
--  | TmTimesfloat of info * term * term
--  | TmTAbs of info * string * ty * term
--  | TmTApp of info * term * ty
--  | TmZero of info
--  | TmSucc of info * term
--  | TmPred of info * term
--  | TmIsZero of info * term
--  | TmInert of info * ty
--  | TmPack of info * ty * term * ty
--  | TmUnpack of info * string * string * term * term

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

--let rec isval ctx t = match t with
--    TmTrue(_)  -> true
--  | TmFalse(_) -> true
--  | TmString _  -> true
--  | TmUnit(_)  -> true
--  | TmFloat _  -> true
--  | t when isnumericval ctx t  -> true
--  | TmAbs(_,_,_,_) -> true
--  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
--  | TmPack(_,_,v1,_) when isval ctx v1 -> true
--  | TmTAbs(_,_,_,_) -> true
--  | _ -> false

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

--let rec isnumericval ctx t = match t with
--    TmZero(_) -> true
--  | TmSucc(_,t1) -> isnumericval ctx t1
--  | _ -> false

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

--type ty =
--    TyVar of int * int
--  | TyId of string
--  | TyTop
--  | TyArr of ty * ty
--  | TyBool
--  | TyRecord of (string * ty) list
--  | TyString
--  | TyUnit
--  | TyFloat
--  | TyAll of string * ty * ty
--  | TyNat
--  | TySome of string * ty * ty
--  | TyAbs of string * kind * ty
--  | TyApp of ty * ty

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

--type binding =
--    NameBind
--  | TyVarBind of ty
--  | VarBind of ty
--  | TyAbbBind of ty * (kind option)
--  | TmAbbBind of term * (ty option)

data Binding = NameBind
             | TypeVarBind Type
             | VarBind Type
             | TypeAddBind Type (Maybe Kind)
             deriving (Show)
