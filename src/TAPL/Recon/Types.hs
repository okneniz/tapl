{-# LANGUAGE FlexibleContexts #-}

module TAPL.Recon.Types where

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
          | TAbs Info String Type Term
          | TApp Info Term Term
          | TBind Info String Binding
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

termMap :: (Int -> Info -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar info name depth) = onVar c info name depth
                           walk c (TAbs info x ty t) = TAbs info x (onType c ty) (walk (c+1) t)
                           walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                           walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                           walk c (TTrue info) = TTrue info
                           walk c (TFalse info) = TFalse info
                           walk c (TZero info) = TZero info
                           walk c (TIsZero info t) = TIsZero info (walk c t)
                           walk c (TPred info t) = TPred info (walk c t)
                           walk c (TSucc info t) = TSucc info (walk c t)
                           walk c x = error $ show x

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onVar (typeShiftAbove d) c t
                 where onVar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onVar c info name depth = TVar info name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType 0 t
                   where onVar c info name depth | name == j + c = termShift c s
                         onVar c info name depth = TVar info name depth
                         onType j ty = ty

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)

data Type = TyBool
          | TyArrow Type Type
          | TyNat
          | TyID String
          | TyVar VarName Depth
          deriving (Eq, Show)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar c ty = walk c ty
               where walk c TyBool = TyBool
                     walk c TyNat = TyNat
                     walk c (TyVar x n) = onVar c x n
                     walk c (TyID x) = TyID x
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)

typeShiftAbove :: Depth -> VarName -> Type -> Type
typeShiftAbove d c ty = typeMap onVar c ty
                  where onVar c name depth | name >= c = TyVar (name + d) (depth + d)
                        onVar c name depth = TyVar name (depth + d)

typeShift :: VarName -> Type -> Type
typeShift d ty = typeShiftAbove d 0 ty

typeSubstitution :: VarName -> Type -> Type -> Type
typeSubstitution j s ty = typeMap onVar 0 ty
                    where onVar c name depth | name == j + c = typeShift c s
                          onVar c name depth = TyVar name depth

typeSubstitutionTop :: Type -> Type -> Type
typeSubstitutionTop s ty = typeShift (-1) (typeSubstitution 0 (typeShift 1 s) ty)

data Binding = NameBind
             | VarBind Type
             deriving (Eq, Show)
