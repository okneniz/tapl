module Language.TAPL.Fomsub.Types where

import Text.Parsec (SourcePos)

import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Kind = Star | Arrow Kind Kind deriving (Show, Eq)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TTAbs SourcePos String Type Term
          | TTApp SourcePos Term Type
          deriving (Show)

type AST = [Term]

isVal :: Term -> Bool
isVal (TAbs _ _ _ _) = True
isVal (TTAbs _ _ _ _) = True
isVal _ = False

typeTermSubstitutionTop tyS t = termShift (-1) (typeTermSubstitution (typeShift 1 tyS) 0 t)

typeTermSubstitution tyS j t = termMap (\p _ x n -> TVar p x n)
                                       (\j tyT -> typeSubstitution tyS j tyT) j t

termMap :: (SourcePos -> Int -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar p name depth) = onVar p c name depth
                           walk c (TAbs p x ty t) = TAbs p x (onType c ty) (walk (c+1) t)
                           walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)
                           walk c (TTAbs p x ty t) = TTAbs p x (onType c ty) (walk (c +1) t)
                           walk c (TTApp p t ty) = TTApp p (walk c t) (onType c ty)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onVar (typeShiftAbove d) c t
                 where onVar p c x n | x >= c = TVar p (x + d) (n + d)
                       onVar p c x n = TVar p x (n + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType j t
                   where onVar p j x n | x == j = termShift j s
                         onVar p j x n = TVar p x n
                         onType j ty = ty

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)

data Type = TyTop
          | TyVar VarName Depth
          | TyArrow Type Type
          | TyAll String Type Type
          | TyAbs String Kind Type
          | TyApp Type Type
          deriving (Show, Eq)

typeMap :: (Int -> VarName -> Depth -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)
                     walk _ TyTop = TyTop
                     walk c (TyVar x n) = onVar c x n
                     walk c (TyAll x ty1 ty2) = TyAll x (walk c ty1) (walk (c+1) ty2)
                     walk c (TyAbs x k ty1) = TyAbs x k (walk (c+1) ty1)
                     walk c (TyApp ty1 ty2) = TyApp (walk c ty1) (walk c ty2)

typeShiftAbove :: Depth -> VarName -> Type -> Type
typeShiftAbove d c ty = typeMap onVar c ty
                  where onVar c x n | x >= c = TyVar (x + d) (n + d)
                        onVar c x n = TyVar x (n + d)

typeShift :: VarName -> Type -> Type
typeShift d tyT = typeShiftAbove d 0 tyT

typeSubstitution :: Type -> VarName -> Type -> Type
typeSubstitution tyS j tyT = typeMap onVar j tyT
                    where onVar j x n | x == j = typeShift j tyS
                          onVar j x n = TyVar x n

typeSubstitutionTop :: Type -> Type -> Type
typeSubstitutionTop tyS tyT = typeShift (-1) (typeSubstitution (typeShift 1 tyS) 0 tyT)

data Binding = NameBind
             | VarBind Type
             | TypeVarBind Type
             deriving (Show)
