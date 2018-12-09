{-# LANGUAGE FlexibleContexts #-}

module TAPL.Untyped.Types where

import Text.Parsec

data Term = TVar Info VarName Depth
          | TAbs Info String Term
          | TApp Info Term Term
          deriving (Show)

type VarName = Int
type Depth = Int
data Info = Info { row :: Int, column :: Int } deriving (Eq)
type AST = [Term]

data Binding = NameBind deriving (Show)
type LCNames = [(String,Binding)]

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

data EvaluationError = ParsecError ParseError deriving (Eq)

instance Show EvaluationError where
    show (ParsecError e) = show e
