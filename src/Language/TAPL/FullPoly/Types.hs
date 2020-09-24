module Language.TAPL.FullPoly.Types where

import Text.Parsec (SourcePos)

import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             | SomeBind SourcePos String String Term
             deriving (Show)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TLet SourcePos String Term Term
          | TFix SourcePos Term
          | TString SourcePos String
          | TUnit SourcePos
          | TAscribe SourcePos Term Type
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term Term
          | TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TFloat SourcePos Double
          | TTimesFloat SourcePos Term Term
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TInt SourcePos Integer
          | TKeyword SourcePos String
          | TPack SourcePos Type Term Type
          | TUnpack SourcePos String String Term Term
          | TTAbs SourcePos String Term
          | TTApp SourcePos Term Type
          deriving (Show)

type AST = [Term]

isVal :: Term -> Bool
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TFloat _ _) = True
isVal x | isNumerical x = True
isVal (TAbs _ _ _ _) = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TPack _ _ v1 _) = isVal v1
isVal (TTAbs _ _ _) = True
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

typeTermSubstitutionTop tyS t = termShift (-1) (typeTermSubstitution (typeShift 1 tyS) 0 t)

typeTermSubstitution tyS j t = termMap (\p _ x n -> TVar p x n)
                                       (\j tyT -> typeSubstitution tyS j tyT) j t

termMap :: (SourcePos -> Int -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar p name depth) = onVar p c name depth
                           walk c (TAbs p x ty t) = TAbs p x (onType c ty) (walk (c+1) t)
                           walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)
                           walk c (TLet p x t1 t2) = TLet p x (walk c t1) (walk (c+1) t2)
                           walk c (TFix p t) = TFix p (walk c t)
                           walk c (TString p s) = TString p s
                           walk c (TUnit p) = TUnit p
                           walk c (TTrue p) = TTrue p
                           walk c (TFalse p) = TFalse p
                           walk c (TIf p t1 t2 t3) = TIf p (walk c t1) (walk c t2) (walk c t3)
                           walk c (TAscribe p t ty) = TAscribe p (walk c t) (onType c ty)
                           walk c (TFloat p t) = TFloat p t
                           walk c (TTimesFloat p t1 t2) = TTimesFloat p (walk c t1) (walk c t2)
                           walk c (TZero p) = TZero p
                           walk c (TSucc p t) = TSucc p (walk c t)
                           walk c (TPred p t) = TPred p (walk c t)
                           walk c (TIsZero p t) = TIsZero p (walk c t)
                           walk c (TInt p t) = TInt p t
                           walk c (TKeyword p t) = TKeyword p t
                           walk c (TRecord p fields) = TRecord p $ Map.map (walk c) fields
                           walk c (TProj p r k) = TProj p (walk c r) k
                           walk c (TPack p ty1 t ty2) = TPack p (onType c ty1) (walk c t) (onType c ty2)
                           walk c (TUnpack p ty x t1 t2) = TUnpack p ty x (walk c t1) (walk (c+2) t2)
                           walk c (TTAbs p x t) = TTAbs p x $ walk (c +1) t
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

data Type = TyVar VarName Depth
          | TyID String
          | TyArrow Type Type
          | TyString
          | TyUnit
          | TyRecord (Map String Type)
          | TyBool
          | TyFloat
          | TyNat
          | TySome String Type
          | TyAll String Type
          | TyKeyword
          | TyInt
          deriving (Show, Eq)

typeMap :: (Int -> VarName -> Depth -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyVar x n) = onVar c x n
                     walk _ (TyID x) = TyID x
                     walk _ TyString = TyString
                     walk _ TyUnit = TyUnit
                     walk _ TyFloat = TyFloat
                     walk _ TyBool = TyBool
                     walk _ TyNat = TyNat
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)
                     walk c (TySome x ty) = TySome x $ walk (c+1) ty
                     walk c (TyAll x ty) = TyAll x $ walk (c+1) ty
                     walk c (TyRecord fs) = TyRecord $ Map.map (walk c) fs
                     walk _ TyInt = TyInt
                     walk _ TyKeyword = TyKeyword

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
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)
