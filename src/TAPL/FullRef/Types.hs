{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullRef.Types where

import Prelude
import qualified Prelude (lookup)

import Text.Parsec (SourcePos)
import Text.Parsec.Error (ParseError(..))
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)
import Data.Function (on)
import Data.List (intercalate, all, sortBy)

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
          | TRecord Info [(String, Term)]
          | TKeyword Info String
          | TFix Info Term
          | TTag Info String Term Type
          | TCase Info Term [(String, String, Term)]
          deriving (Show)

type VarName = Int
type Depth = Int
type Info = Maybe SourcePos
type Location = Int
type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)
type LCNames = [(String,Binding)]
data LCMemory = LCMemory [Term]

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
          | TyRecord [(String, Type)]
          | TyKeyword
          | TyVariant [(String, Type)]

instance Show Type where
    show TyBool = "Bool"
    show TyString = "String"
    show TyTop = "Top"
    show TyBot = "Bot"
    show TyUnit = "Unit"
    show TyNat = "Nat"
    show TyInt = "Int"
    show TyFloat = "Float"
    show TyKeyword = "Keyword"
    show (TyArrow ty1 ty2) = show ty1 ++ " -> " ++ show ty2
    show (TyRef t) = "Ref " ++ show t
    show (TyID s) = s
    show (TyProduct t1 t2) = "{" ++ show t1 ++ "*" ++ show t2 ++ "}"
    show (TyRecord ts) =  "{" ++ (intercalate ", " $ (\(k,ty) -> k ++ "=" ++ show ty) <$> ts) ++ "}"
    show (TyVariant ts) = "<" ++ (intercalate ", " $ map field ts) ++ ">"
                    where field (k,t) = k ++ ":" ++ show t

data EvaluationError = ParsecError ParseError
                     | InvalidOperation Info String
                     | TypeError String
                     deriving (Eq)

instance Show EvaluationError where
    show (ParsecError e) = show e
    show (InvalidOperation info s) = show info
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
  TyBool == TyBool = True
  TyString == TyString = True
  TyUnit == TyUnit = True
  TyNat == TyNat = True
  TyFloat == TyFloat = True
  TyInt == TyInt = True
  (TyID x) == (TyID y) = x == y
  TyTop == TyTop = True
  TyBot == TyBot = True
  (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)
  (TyRef tyT1) == (TyRef tyT2) = tyT1 == tyT2
  (TyProduct tyT1 tyT2) == (TyProduct tyT1' tyT2') = (tyT1 == tyT2) && (tyT1' == tyT2')
  (TyRecord tys1) == (TyRecord tys2) = ((length tys1) == (length tys2)) && (all eqPair $ zip (sortBy ordPair tys1) (sortBy ordPair tys2))
               where eqPair ((x, ty1), (y, ty2)) = (x == y) && (ty1 == ty2)
  (TyVariant tys1) == (TyVariant tys2) = ((length tys1) == (length tys2)) && (all eqPair $ zip (sortBy ordPair tys1) (sortBy ordPair tys2))
                where eqPair ((x, ty1), (y, ty2)) = (x == y) && (ty1 == ty2)
  _ == _ = False

(<:) :: Type -> Type -> Bool
_ <: TyTop = True
TyBot <: _ = True
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
(TyRef tyT1) <: (TyRef tyT2) = (tyT1 <: tyT2) && (tyT2 <: tyT1)
(TyProduct tyS1 tyS2) <: (TyProduct tyT1 tyT2) = (tyS1 <: tyT1) && (tyS2 <: tyT2)
(TyRecord ty1) <: (TyRecord ty2) = all f ty2
                             where f (k,ty) = case Prelude.lookup k ty1 of
                                                   (Just x) -> x <: ty
                                                   Nothing -> False
(TyVariant ty1) <: (TyVariant ty2) = all f ty2
                               where f (k,ty) = case Prelude.lookup k ty1 of
                                                     (Just x) -> x <: ty
                                                     Nothing -> False
x <: y | x == y = True
x <: y = False

ordPair = compare `on` fst

class Memory m where
    extend :: m -> Term -> (Int, m)
    lookup :: m -> Location -> Term
    update :: m -> Location -> Term -> m
    size :: m -> Int

instance Memory LCMemory where
    extend (LCMemory s) t = (length s, LCMemory (s ++ [t]))
    lookup (LCMemory s) l = s !! l
    update (LCMemory s) l t =
        let f 0 (_:rest) = t:rest
            f location (t':rest) = t':(f (location - 1) rest)
            f _ _ = error "invalid location"
        in LCMemory $ f l s
    size (LCMemory s) = length s

instance Show LCMemory where
    show (LCMemory x) = "<memory: " ++ show x ++ " >"
