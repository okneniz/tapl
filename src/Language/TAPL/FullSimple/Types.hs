module Language.TAPL.FullSimple.Types where

import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Text.Parsec (SourcePos)
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TVar SourcePos VarName Depth
          | TInt SourcePos Integer
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TString SourcePos String
          | TFloat SourcePos Double
          | TUnit SourcePos
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TPair SourcePos Term Term
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term Term
          | TLet SourcePos String Term Term
          | TAscribe SourcePos Term Type
          | TCase SourcePos Term (Map String (String, Term))
          | TTag SourcePos String Term Type
          | TKeyword SourcePos String
          | TFix SourcePos Term
          | TTimesFloat SourcePos Term Term
          deriving (Show)

type Location = Int
type AST = [Term]

data Binding = NameBind
             | VarBind Type
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TFloat _ _) = True
isVal x | isNumerical x = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal (TAscribe _ t _) = isVal t
isVal (TTag _ _ t _) = isVal t
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

tmmap :: (Int -> SourcePos -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar p name depth) = onvar c p name depth
                  walk c (TAbs p x ty t1) = TAbs p x ty (walk (c+1) t1)
                  walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)
                  walk c (TIf p t1 t2 t3) = TIf p (walk c t1) (walk c t2) (walk c t3)
                  walk _ (TTrue p) = TTrue p
                  walk _ (TFalse p) = TFalse p
                  walk _ (TString p x) = TString p x
                  walk _ (TUnit p) = TUnit p
                  walk _ (TZero p) = TZero p
                  walk c (TIsZero p t1) = TIsZero p (walk c t1)
                  walk c (TPred p t1) = TPred p (walk c t1)
                  walk c (TSucc p t1) = TSucc p (walk c t1)
                  walk _ (TFloat p t1) = TFloat p t1
                  walk _ (TInt p t1) = TInt p t1
                  walk c (TPair p t1 t2) = TPair p (walk c t1) (walk c t2)
                  walk c (TRecord p fields) = TRecord p $ Map.map (walk c) fields
                  walk c (TTag p k t1 ty) = TTag p k (walk c t1) ty
                  walk c (TProj p r k) = TProj p (walk c r) k
                  walk c (TLet p x t1 t2) = TLet p x (walk c t1) (walk (c+1) t2)
                  walk c (TAscribe p t1 ty) = TAscribe p (walk c t1) ty
                  walk c (TCase p t1 branches) = TCase p (walk c t1) $ Map.map walkBranch branches
                                              where walkBranch (x, y) = (x, walk (c + 1) y)
                  walk c (TFix p t1) = TFix p (walk c t1)
                  walk _ t1@(TKeyword _ _) = t1

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = tmmap onvar s t
                 where onvar c p name depth | name >= c = TVar p (name + d) (depth + d)
                       onvar _ p name depth = TVar p name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c _ name _ | name == j + c = shift c s
                     onvar _ p name depth = TVar p name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)

data Type = TyBool
          | TyArrow Type Type
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyInt
          | TyProduct Type Type
          | TyRecord (Map String Type)
          | TyID String
          | TyVariant (Map String Type)
          | TyKeyword
          | TyVar VarName Depth
          deriving (Show)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk _ TyBool = TyBool
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)
                     walk _ TyString = TyString
                     walk _ TyUnit = TyUnit
                     walk _ TyNat = TyNat
                     walk _ TyFloat = TyFloat
                     walk c (TyRecord fs) = TyRecord $ Map.map (walk c) fs
                     walk _ x@(TyID _) = x
                     walk c (TyVariant fs) = TyVariant $ Map.map (walk c) fs
                     walk c (TyVar x n) = onVar c x n

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
