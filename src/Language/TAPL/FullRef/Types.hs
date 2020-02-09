module Language.TAPL.FullRef.Types where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Term = TTrue Info
          | TFalse Info
          | TString Info String
          | TFloat Info Double
          | TInt Info Integer
          | TUnit Info
          | TZero Info
          | TSucc Info Term
          | TPred Info Term
          | TIsZero Info Term
          | TIf Info Term Term Term
          | TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          | TRef Info Term
          | TDeref Info Term
          | TAssign Info Term Term
          | TLoc Info Location
          | TLet Info String Term Term
          | TAscribe Info Term Type
          | TPair Info Term Term
          | TLookup Info Term Term
          | TRecord Info (Map String Term)
          | TKeyword Info String
          | TFix Info Term
          | TTag Info String Term Type
          | TCase Info Term (Map String (String, Term))
          deriving (Show)

data Type = TyBool
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyInt
          | TyArrow Type Type
          | TyRef Type
          | TyID String
          | TyTop
          | TyBot
          | TyProduct Type Type
          | TyRecord (Map String Type)
          | TyKeyword
          | TyVariant (Map String Type)
          deriving (Show)

type VarName = Int
type Depth = Int
type AST = [Term]

data Info = Info { row :: Int, column :: Int } deriving (Show)

instance Eq Type where
  TyBool == TyBool = True
  TyString == TyString = True
  TyUnit == TyUnit = True
  TyNat == TyNat = True
  TyFloat == TyFloat = True
  TyInt == TyInt = True
  (TyID x) == (TyID y) = x == y
  TyTop == TyTop = True
  TyBot == TyBot = True
  (TyRef tyT1) == (TyRef tyT2) = tyT1 == tyT2
  (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)
  (TyProduct tyT1 tyT2) == (TyProduct tyT1' tyT2') = (tyT1 == tyT2) && (tyT1' == tyT2')
  (TyRecord tys1) == (TyRecord tys2) = tys1 == tys2
  (TyVariant tys1) == (TyVariant tys2) = tys1 == tys2
  _ == _ = False

(<:) :: Type -> Type -> Bool
_ <: TyTop = True
TyBot <: _ = True
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
(TyRef tyT1) <: (TyRef tyT2) = (tyT1 <: tyT2) && (tyT2 <: tyT1)
(TyProduct tyS1 tyS2) <: (TyProduct tyT1 tyT2) = (tyS1 <: tyT1) && (tyS2 <: tyT2)

(TyRecord ty1) <: (TyRecord ty2) =
    all f $ Map.elems $ Map.intersectionWith (,) ty1 $ Map.filterWithKey (\k _ -> Map.member k ty1) ty2
    where f (ty1', ty2') = ty1' <: ty2'

(TyVariant ty1) <: (TyVariant ty2) =
    all f $ Map.elems $ Map.intersectionWith (,) ty1 $ Map.filterWithKey (\k _ -> Map.member k ty1) ty2
    where f (ty1', ty2') = ty1' <: ty2'

x <: y | x == y = True
_ <: _ = False

type LCMemory = [Term]
type Location = Int

extend :: LCMemory -> Term -> (Location, LCMemory)
extend s t = (size s, (s ++ [t]))

lookup :: LCMemory -> Location -> Term
lookup s l = s !! l

update :: LCMemory -> Location -> Term -> LCMemory
update s l t =
    let f 0 (_:rest) = t:rest
        f location (t':rest) = t':(f (location - 1) rest)
        f _ _ = error "invalid location"
    in f l s

size :: LCMemory -> Int
size = length
