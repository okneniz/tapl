module Language.TAPL.FullError.Types where

import Language.TAPL.Common.Context
import Text.Parsec (SourcePos)

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TError SourcePos
          | TTry SourcePos Term Term
          deriving (Show, Eq)

type AST = [Term]

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar info name depth) = onVar c info name depth
                           walk c (TAbs info x ty t) = TAbs info x (onType c ty) (walk (c+1) t)
                           walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                           walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                           walk _ (TTrue info) = TTrue info
                           walk _ (TFalse info) = TFalse info
                           walk _ (TError info) = TError info
                           walk c (TTry info t1 t2) = TTry info (walk c t1) (walk c t2)

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

data Type = TyArrow Type Type
          | TyBool
          | TyBot
          | TyTop
          | TyVar VarName Depth
          deriving (Show, Eq)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk _ TyBool = TyBool
                     walk _ TyTop = TyTop
                     walk _ TyBot = TyBot
                     walk c (TyVar x n) = onVar c x n
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
