{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullSimple.Types where

import Prelude hiding (abs, succ, pred)
import Control.Monad
import Control.Applicative hiding ((<|>), many, optional)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim (try)
import Data.List (findIndex, intercalate, all, nub, (\\), sortBy)
import Data.Function (on)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

import Data.Map (Map)
import qualified Data.Map.Strict as Map

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
          | TRecord Info (Map String Term)
          | TLookup Info Term Term
          | TLet Info String Term Term
          | TAscribe Info Term Type
          | TCase Info Term (Map String (String, Term))
          | TTag Info String Term Type
          | TKeyword Info String
          | TFix Info Term
          deriving (Show)

data Type = TyBool
          | TyArrow Type Type
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyInt
          | TyProduct Type Type
          | TyRecord (Map String Type)
          | TyID String
          | TyVariant (Map String Type)
          | TyKeyword
          | TyTop
          | TyBot
          deriving (Show)

type VarName = Int
type Depth = Int
type Location = Int
type AST = [Term]

data Info = Info { row :: Int, column :: Int } deriving (Show)

data Binding = NameBind | VarBind Type deriving (Show)
