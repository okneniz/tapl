module Language.TAPL.RcdSubBot.Types where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.Parsec (SourcePos)

data Command = Eval [Term]
             | Bind SourcePos String Binding

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term Term
          | TKeyword SourcePos String
          deriving (Eq)

type VarName = Int
type Depth = Int
type AST = [Term]

data Binding = NameBind
             | VarBind Type
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)

data Type = TyArrow Type Type
          | TyTop
          | TyBot
          | TyRecord (Map String Type)
          deriving (Show, Eq)

isVal :: Term -> Bool
isVal (TAbs _ _ _ _) = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> Int -> Term -> Term
termMap onVar = walk
          where walk c (TVar pos name depth) = onVar c pos name depth
                walk c (TAbs pos x ty t1) = TAbs pos x ty (walk (c+1) t1)
                walk c (TApp pos t1 t2) = TApp pos (walk c t1) (walk c t2)
                walk c (TRecord pos fields) = TRecord pos $ Map.map (walk c) fields
                walk c (TProj pos t1 k) = TProj pos (walk c t1) k

termShiftAbove :: Depth -> Int -> Term -> Term
termShiftAbove d = termMap onVar
             where onVar c pos name depth | name >= c = TVar pos (name + d) (depth + d)
                   onVar _ pos name depth = TVar pos name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s = termMap onVar 0
                 where onVar c _ name _ | name == j + c = termShift c s
                       onVar _ pos name depth = TVar pos name depth

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
