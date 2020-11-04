module Language.TAPL.FullIsorec.Types where

import Text.Parsec (SourcePos)

import Data.List (all)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TLet SourcePos String Term Term
          | TFix SourcePos Term
          | TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TString SourcePos String
          | TUnit SourcePos
          | TAscribe SourcePos Term Type
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term String
          | TFloat SourcePos Double
          | TTimesFloat SourcePos Term Term
          | TFold SourcePos Type
          | TUnfold SourcePos Type
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TCase SourcePos Term (Map String (String, Term))
          | TTag SourcePos String Term Type
          | TPair SourcePos Term Term
          deriving (Show)

type AST = [Term]

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TFloat _ _) = True
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal x | isNumerical x = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TAscribe _ t _) = isVal t
isVal (TTag _ _ t _) = isVal t
isVal (TApp _ (TFold _ _) v) = isVal v
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar p name depth) = onVar c p name depth
                           walk c (TAbs p x ty t) = TAbs p x (onType c ty) (walk (c+1) t)
                           walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)
                           walk c (TIf p t1 t2 t3) = TIf p (walk c t1) (walk c t2) (walk c t3)
                           walk c (TTrue p) = TTrue p
                           walk c (TFalse p) = TFalse p
                           walk c (TString p s) = TString p s
                           walk c (TUnit p) = TUnit p
                           walk c (TZero p) = TZero p
                           walk c (TIsZero p t) = TIsZero p (walk c t)
                           walk c (TPred p t) = TPred p (walk c t)
                           walk c (TSucc p t) = TSucc p (walk c t)
                           walk c (TFloat p t) = TFloat p t
                           walk c (TFix p t) = TFix p (walk c t)
                           walk c (TPair p t1 t2) = TPair p (walk c t1) (walk c t2)
                           walk c (TRecord p fields) = TRecord p $ Map.map (walk c) fields
                           walk c (TTag p k t ty) = TTag p k (walk c t) (onType c ty)
                           walk c (TProj p r k) = TProj p (walk c r) k
                           walk c (TLet p x t1 t2) = TLet p x (walk c t1) (walk (c+1) t2)
                           walk c (TAscribe p t ty) = TAscribe p (walk c t) (onType c ty)
                           walk c (TTimesFloat p t1 t2) = TTimesFloat p (walk c t1) (walk c t2)
                           walk c (TFold p ty) = TFold p (onType c ty)
                           walk c (TUnfold p ty) = TUnfold p (onType c ty)
                           walk c (TCase p t1 branches) = TCase p (walk c t1) $ Map.map walkBranch branches
                                                       where walkBranch (x, y) = (x, walk (c + 1) y)

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

data Type = TyVar VarName Depth
          | TyID String
          | TyArrow Type Type
          | TyUnit
          | TyRecord (Map String Type)
          | TyFloat
          | TyRec String Type
          | TyNat
          | TyVariant (Map String Type)
          | TyString
          | TyBool
          | TyProduct Type Type
          deriving (Show, Eq)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyVar x n) = onVar c x n
                     walk _ (TyID x) = TyID x
                     walk _ TyString = TyString
                     walk _ TyUnit = TyUnit
                     walk c (TyRecord fs) = TyRecord $ Map.map (walk c) fs
                     walk _ TyFloat = TyFloat
                     walk c (TyRec x ty1) = TyRec x (walk (c+1) ty1)
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)
                     walk _ TyBool = TyBool
                     walk _ TyNat = TyNat
                     walk c (TyVariant fs) = TyVariant $ Map.map (walk c) fs
                     walk c (TyProduct ty1 ty2) = TyProduct (walk c ty1) (walk c ty2)

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
