{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullRef.Types where

import Text.Parsec (SourcePos)
import Text.Parsec.Error (ParseError(..))
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)

data Term = TTrue Info
          | TFalse Info
          | TString Info String
          | TFloat Info Double
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
          deriving (Show, Eq)

type VarName = Int
type Depth = Int
type Info = Maybe SourcePos
type LCNames = [(String,Binding)]
type Location = Int
type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)

data Type = TyBool
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyArrow Type Type
          | TyRef Type
          deriving (Eq)

instance Show Type where
    show TyBool = "Bool"
    show TyString = "String"
    show TyUnit = "Unit"
    show TyNat = "Nat"
    show TyFloat = "Float"
    show (TyArrow ty1 ty2) = show ty1 ++ " -> " ++ show ty2
    show (TyRef t) = "Ref " ++ show t

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

