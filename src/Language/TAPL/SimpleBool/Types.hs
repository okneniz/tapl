{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.SimpleBool.Types where

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

data Term = TTrue Info
          | TFalse Info
          | TIf Info Term Term Term
          | TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          deriving (Show)

data Type = TyBool
          | TyArrow Type Type
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

data EvaluationError = ParsecError ParseError
                     | TypeError String
                     deriving (Eq)

instance Show EvaluationError where
    show (ParsecError e) = show e
    show (TypeError s) = show s

data TypeError = TypeMissmatch Info String

instance Show TypeError where
    show (TypeMissmatch info message) = message ++ " in " ++ (show $ row info) ++ ":" ++ (show $ column info)