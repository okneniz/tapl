module Language.TAPL.FullRef.Types where

import Text.Parsec (SourcePos)
import Language.TAPL.Common.Context

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Binding = NameBind
             | VarBind Type
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)

data Term = TTrue SourcePos
          | TFalse SourcePos
          | TString SourcePos String
          | TFloat SourcePos Double
          | TInt SourcePos Integer
          | TUnit SourcePos
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TIf SourcePos Term Term Term
          | TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TRef SourcePos Term
          | TDeref SourcePos Term
          | TAssign SourcePos Term Term
          | TLoc SourcePos Location
          | TLet SourcePos String Term Term
          | TAscribe SourcePos Term Type
          | TPair SourcePos Term Term
          | TProj SourcePos Term Term
          | TRecord SourcePos (Map String Term)
          | TKeyword SourcePos String
          | TFix SourcePos Term
          | TTag SourcePos String Term Type
          | TCase SourcePos Term (Map String (String, Term))
          | TTimesFloat SourcePos Term Term
          deriving (Show)

type AST = [Term]
type Location = Int

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TTag _ _ t _) = isVal t
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TLoc _ _) = True
isVal (TFloat _ _) = True
isVal t | isNumerical t = True
isVal (TAbs _ _ _ _) = True
isVal (TInt _ _) = True
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ t) = isNumerical t
isNumerical _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
              where walk c (TVar p name depth) = onVar c p name depth
                    walk c (TAbs p x ty t) = TAbs p x (onType c ty) (walk (c+1) t)
                    walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)
                    walk _ (TTrue p) = TTrue p
                    walk _ (TFalse p) = TFalse p
                    walk c (TIf p t1 t2 t3) = TIf p (walk c t1) (walk c t2) (walk c t3)
                    walk c (TLet p v t1 t2) = TLet p v (walk c t1) (walk (c+1) t2)
                    walk c (TFix p t1) = TFix p (walk c t1)
                    walk c (TProj p t1 t2) = TProj p (walk c t1) (walk c t2)
                    walk c (TRecord p fs) = TRecord p $ Map.map (walk c) fs
                    walk c (TTag p k t ty) = TTag p k (walk c t) (onType c ty)
                    walk c (TCase p t1 bs) = TCase p (walk c t1) $ Map.map f bs where f (x, y) = (x, walk (c+1) y)
                    walk c (TAscribe p t ty) = TAscribe p (walk c t) (onType c ty)
                    walk _ (TString p x) = TString p x
                    walk _ (TUnit p) = TUnit p
                    walk _ t1@(TLoc _ _) = t1
                    walk c (TRef p t1) = TRef p (walk c t1)
                    walk c (TDeref p t1) = TDeref p (walk c t1)
                    walk c (TAssign p t1 t2) = TAssign p (walk c t1) (walk c t2)
                    walk _ (TFloat p t1) = TFloat p t1
                    walk c (TTimesFloat p t1 t2) = TTimesFloat p (walk c t1) (walk c t2)
                    walk _ (TZero p) = TZero p
                    walk c (TSucc p t1) = TSucc p (walk c t1)
                    walk c (TPred p t1) = TPred p (walk c t1)
                    walk c (TIsZero p t1) = TIsZero p (walk c t1)
                    walk _ (TInt p t1) = TInt p t1
                    walk c (TPair p t1 t2) = TPair p (walk c t1) (walk c t2)
                    walk _ t1@(TKeyword _ _) = t1

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

data Type = TyBool
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyInt
          | TyArrow Type Type
          | TyRef Type
          | TySource Type
          | TySink Type
          | TyID String
          | TyTop
          | TyBot
          | TyProduct Type Type
          | TyRecord (Map String Type)
          | TyKeyword
          | TyVariant (Map String Type)
          | TyVar VarName Depth
          deriving (Show)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyVar x n) = onVar c x n
                     walk _ (TyID x) = TyID x
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)
                     walk _ TyTop = TyTop
                     walk _ TyBool = TyBool
                     walk c (TyRecord fs) = TyRecord $ Map.map (walk c) fs
                     walk c (TyVariant fs) = TyVariant $ Map.map (walk c) fs
                     walk _ TyBot = TyBot
                     walk _ TyString = TyString
                     walk _ TyFloat = TyFloat
                     walk _ TyUnit = TyUnit
                     walk c (TyRef ty1) = TyRef (walk c ty1)
                     walk c (TySource ty1) = TySource (walk c ty1)
                     walk c (TySink ty1) = TySink (walk c ty1)
                     walk _ TyNat = TyNat
                     walk _ TyInt = TyInt
                     walk _ TyKeyword = TyKeyword
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
