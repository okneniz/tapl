{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullRecon.Types where

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
          | TZero Info
          | TSucc Info Term
          | TPred Info Term
          | TIsZero Info Term
          | TVar Info VarName Depth
          | TAbs Info String (Maybe Type) Term
          | TApp Info Term Term
          | TBind Info String Binding
          | TLet Info String Term Term
          deriving (Eq, Show)

data Type = TyBool
          | TyArrow Type Type
          | TyNat
          | TyID String
          | TyVar VarName Depth
          deriving (Eq, Show)

data Binding = NameBind
             | VarBind Type
             deriving (Eq, Show)

type VarName = Int
type Depth = Int

data Info = Info { row :: Int, column :: Int } deriving (Eq)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal x | isNumerical x = True
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

termMap :: (Int -> Info -> VarName -> Depth -> Term) -> Int -> Term -> Term
termMap onVar s t = walk s t
              where walk c (TVar info name depth) = onVar c info name depth
                    walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
                    walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                    walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                    walk c (TTrue info) = TTrue info
                    walk c (TFalse info) = TFalse info
                    walk c (TZero info) = TZero info
                    walk c (TIsZero info t) = TIsZero info (walk c t)
                    walk c (TPred info t) = TPred info (walk c t)
                    walk c (TSucc info t) = TSucc info (walk c t)
                    walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)
                    walk c x = error $ show x

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onVar c t
                 where onVar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onVar c info name depth = TVar info name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onvar 0 t
                   where onvar c info name depth | name == j = termShift c s
                         onvar c info name depth = TVar info name depth

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
