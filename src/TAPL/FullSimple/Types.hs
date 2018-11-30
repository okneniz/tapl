{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullSimple.Types where

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intercalate, all, nub, (\\))
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

data Term = TTrue Info
          | TFalse Info
          | TIf Info Term Term Term
          | TVar Info VarName Depth
          | TInt Info Integer
          | TAbs Info String Type Term
          | TApp Info Term Term
          | TString Info String
          | TFloat Info Double
          | TUnit Info
          | TZero Info
          | TSucc Info Term
          | TPred Info Term
          | TIsZero Info Term
          | TPair Info Term Term
          | TRecord Info [(String, Term)]
          | TLookup Info Term Term
          | TLet Info String Term Term
          | TAscribe Info Term Type
          | TCase Info Term [(String, String, Term)]
          | TTag Info String Term Type
          | TKeyword Info String
          | TFix Info Term
          deriving (Show, Eq)

data Type = TyBool
          | TyArrow Type Type
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyInt
          | TyProduct Type Type
          | TyRecord [(String, Type)]
          | TyID String
          | TyVariant [(String, Type)]
          | TyKeyword
          deriving (Eq)

type VarName = Int
type Depth = Int
data Info = Info { row :: Int, column :: Int } deriving (Eq)
type Location = Int
type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)
type LCNames = [(String,Binding)]
data Pattern = Pattern String String deriving (Show, Eq)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

instance Show Type where
    show (TyArrow t1 t2) = "("++ show t1 ++ " -> " ++ show t2 ++ ")"
    show TyBool = "Bool"
    show TyInt = "Int"
    show TyString = "String"
    show TyUnit = "Unit"
    show TyNat = "Nat"
    show TyFloat = "Float"
    show TyKeyword = "Keyword"
    show (TyProduct t1 t2) = "{" ++ show t1 ++ "*" ++ show t2 ++ "}"
    show (TyRecord ts) = "{" ++ (intercalate ", " $ map field ts) ++ "}"
                   where field (k,t) = k ++ "=" ++ show t
    show (TyID s) = s
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
    show (TypeMissmatch info message) = message ++ " in " ++ (show $ row info) ++ ":" ++ (show $ column info)
