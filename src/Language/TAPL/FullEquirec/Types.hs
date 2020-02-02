{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullEquirec.Types where

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
          | TBind Info String Binding
          | TTimesFloat Info Term Term
           deriving (Show)

type VarName = Int
type Depth = Int

data Info = Info { row :: Int, column :: Int } deriving (Eq)

instance Show Info where
    show info = (show $ row info) ++ ":" ++ (show $ column info)

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TFloat _ _) = True
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal x | isNumerical x = True
isVal (TRecord _ ts) = all (\(_,y) -> isVal y) ts
isVal (TAscribe _ t _) = isVal t
isVal (TTag _ _ t _) = isVal t
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
                           walk c (TString info s) = TString info s
                           walk c (TUnit info) = TUnit info
                           walk c (TZero info) = TZero info
                           walk c (TInt info t) = TInt info t
                           walk c (TKeyword info t) = TKeyword info t
                           walk c (TIsZero info t) = TIsZero info (walk c t)
                           walk c (TPred info t) = TPred info (walk c t)
                           walk c (TSucc info t) = TSucc info (walk c t)
                           walk c (TFloat info t) = TFloat info t
                           walk c (TFix info t) = TFix info (walk c t)
                           walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                           walk c (TRecord info fields) = TRecord info $ (\(k,v) -> (k, walk c v)) <$> fields
                           walk c (TTag info k t ty) = TTag info k (walk c t) (onType c ty)
                           walk c (TLookup info r k) = TLookup info (walk c r) k
                           walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)
                           walk c (TAscribe info t ty) = TAscribe info (walk c t) (onType c ty)
                           walk c (TTimesFloat info t1 t2) = TTimesFloat info (walk c t1) (walk c t2)
                           walk c (TCase info t branches) = TCase info (walk c t) $ walkBranch <$> branches
                                                      where walkBranch (k, v, x) = (k, v, walk (c + 1) x)

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
          | TyTop
          | TyBot
          | TyVar VarName Depth
          | TyRec String Type
          deriving (Show, Eq)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar c ty = walk c ty
               where walk c TyString = TyString
                     walk c TyBool = TyBool
                     walk c TyUnit = TyUnit
                     walk c TyNat = TyNat
                     walk c TyFloat = TyFloat
                     walk c TyInt = TyInt
                     walk c TyTop = TyTop
                     walk c TyBot = TyBot
                     walk c TyKeyword = TyKeyword
                     walk c (TyVar x n) = onVar c x n
                     walk c (TyRec x ty1) = TyRec x (walk (c+1) ty1)
                     walk c (TyID x) = TyID x
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)
                     walk c (TyProduct ty1 ty2) = TyProduct (walk c ty1) (walk c ty2)
                     walk c (TyRecord fs) = TyRecord $ (\(x, ty) -> (x, walk c ty)) <$> fs
                     walk c (TyVariant fs) = TyVariant $ (\(x, ty) -> (x, walk c ty)) <$> fs

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
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)

bindingShift :: VarName -> Binding -> Binding
bindingShift d NameBind = NameBind
bindingShift d (VarBind ty) = VarBind (typeShift d ty)
bindingShift d TypeVarBind = TypeVarBind
bindingShift d (TypeAddBind ty) = TypeAddBind (typeShift d ty)
