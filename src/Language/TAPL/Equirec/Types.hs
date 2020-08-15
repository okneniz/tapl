module Language.TAPL.Equirec.Types where

import Text.Parsec (SourcePos)
import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          deriving (Show)

type AST = [Term]
type VarName = Int
type Depth = Int

isVal :: Term -> Bool
isVal (TAbs _ _ _ _) = True
isVal _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar p name depth) = onVar c p name depth
                           walk c (TAbs p x ty t) = TAbs p x (onType c ty) (walk (c+1) t)
                           walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onVar (typeShiftAbove d) c t
                 where onVar c p name depth | name >= c = TVar p (name + d) (depth + d)
                       onVar c p name depth = TVar p name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType 0 t
                   where onVar c p name depth | name == j + c = termShift c s
                         onVar c p name depth = TVar p name depth
                         onType j ty = ty

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)

data Type = TyArrow Type Type
          | TyID String
          | TyVar VarName Depth
          | TyRec String Type
          deriving (Show, Eq)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyVar x n) = onVar c x n
                     walk c (TyRec x ty1) = TyRec x (walk (c+1) ty1)
                     walk _ (TyID x) = TyID x
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
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)
