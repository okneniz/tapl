{-# LANGUAGE FlexibleInstances #-}

module TAPL.Bot.Types where

import Text.Parsec (SourcePos)
import Text.Parsec.Error (ParseError(..))
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)

data Term = TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          deriving (Show, Eq)

type VarName = Int
type Depth = Int
type Info = Maybe SourcePos
type LCNames = [(String,Binding)]
type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)

data Type = TyArrow Type Type
          | TyTop
          | TyBot
          deriving (Eq)

instance Show Type where
    show (TyArrow ty1 ty2) = show ty1 ++ " -> " ++ show ty2
    show TyTop = "Top"
    show TyBot = "Bot"

data EvaluationError = ParsecError ParseError
                     | TypeError String
                     deriving (Eq)

instance Show EvaluationError where
    show (ParsecError e) = show e
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

