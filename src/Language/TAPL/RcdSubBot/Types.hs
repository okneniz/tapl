module Language.TAPL.RcdSubBot.Types where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Command = Eval [Term]
             | Bind Info String Binding

data Term = TVar Info VarName Depth
          | TAbs Info String Type Term
          | TApp Info Term Term
          | TRecord Info (Map String Term)
          | TProj Info Term Term
          | TKeyword Info String
          deriving (Eq)

type VarName = Int
type Depth = Int
type AST = [Term]

data Info = Info { row :: Int, column :: Int } deriving (Eq)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

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

termMap :: (Int -> Info -> VarName -> Depth -> Term) -> Int -> Term -> Term
termMap onVar = walk
          where walk c (TVar info name depth) = onVar c info name depth
                walk c (TAbs info x ty t1) = TAbs info x ty (walk (c+1) t1)
                walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                walk c (TRecord info fields) = TRecord info $ Map.map (walk c) fields
                walk c (TProj info t1 k) = TProj info (walk c t1) k

termShiftAbove :: Depth -> Int -> Term -> Term
termShiftAbove d = termMap onVar
             where onVar c info name depth | name >= c = TVar info (name + d) (depth + d)
                   onVar _ info name depth = TVar info name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s = termMap onVar 0
                 where onVar c _ name _ | name == j + c = termShift c s
                       onVar _ info name depth = TVar info name depth

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
