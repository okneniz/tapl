module Language.TAPL.FullSub.Types where

import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Text.Parsec (SourcePos)
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TRecord SourcePos (Map String Term)
          | TProj SourcePos Term Term
          | TLet SourcePos String Term Term
          | TFix SourcePos Term
          | TString SourcePos String
          | TUnit SourcePos
          | TAscribe SourcePos Term Type
          | TFloat SourcePos Double
          | TTimesFloat SourcePos Term Term
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          | TKeyword SourcePos String
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
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

--let tytermSubstTop tyS t =
--  termShift (-1) (tytermSubst (typeShift 1 tyS) 0 t)

typeTermSubstitutionTop tyS t = termShift (-1) (typeTermSubstitution (typeShift 1 tyS) 0 t)

--let rec tytermSubst tyS j t =
--  tmmap (fun fi c x n -> TmVar(fi,x,n))
--        (fun j tyT -> typeSubst tyS j tyT) j t

typeTermSubstitution tyS j t = termMap (\p _ x n -> TVar p x n)
                                       (\j tyT -> typeSubstitution tyS j tyT) j t

termMap :: (SourcePos -> Int -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
     where walk c (TVar p name depth) = onVar p c name depth
           walk c (TAbs p x ty t) = TAbs p x (onType c ty) (walk (c+1) t)
           walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)
           walk c (TTrue p) = TTrue p
           walk c (TFalse p) = TFalse p
           walk c (TIf p t1 t2 t3) = TIf p (walk c t1) (walk c t2) (walk c t3)
           walk c (TProj p r k) = TProj p (walk c r) k
           walk c (TRecord p fields) = TRecord p $ Map.map (walk c) fields
           walk c (TLet p x t1 t2) = TLet p x (walk c t1) (walk (c+1) t2)
           walk c (TFix p t) = TFix p (walk c t)
           walk c (TString p s) = TString p s
           walk c (TUnit p) = TUnit p
           walk c (TAscribe p t ty) = TAscribe p (walk c t) (onType c ty)
           walk c (TFloat p t) = TFloat p t
           walk c (TTimesFloat p t1 t2) = TTimesFloat p (walk c t1) (walk c t2)
           walk c (TZero p) = TZero p
           walk c (TSucc p t) = TSucc p (walk c t)
           walk c (TPred p t) = TPred p (walk c t)
           walk c (TIsZero p t) = TIsZero p (walk c t)
           walk c (TKeyword p t) = TKeyword p t

--let termShiftAbove d c t =
--  tmmap
--    (fun fi c x n -> if x>=c then TmVar(fi,x+d,n+d)  else TmVar(fi,x,n+d))
--    (typeShiftAbove d)
--    c t

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onVar (typeShiftAbove d) c t
                 where onVar p c x n | x >= c = TVar p (x + d) (n + d)
                       onVar p c x n = TVar p x (n + d)

--let termShift d t = termShiftAbove d 0 t

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

--let termSubst j s t =
--  tmmap
--    (fun fi j x n -> if x=j then termShift j s else TmVar(fi,x,n))
--    (fun j tyT -> tyT)
--    j t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType j t
                   where onVar p j x n | x == j = termShift j s
                         onVar p j x n = TVar p x n
                         onType j ty = ty

--let termSubstTop s t =
--  termShift (-1) (termSubst 0 (termShift 1 s) t)

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)

data Type = TyVar VarName Depth
          | TyID String
          | TyTop
          | TyArrow Type Type
          | TyBool
          | TyRecord (Map String Type)
          | TyString
          | TyUnit
          | TyFloat
          | TyNat
          | TyKeyword
          deriving (Show)

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyVar x n) = onVar c x n
                     walk _ x@(TyID _) = x
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)
                     walk _ TyTop = TyTop
                     walk _ TyBool = TyBool
                     walk c (TyRecord fs) = TyRecord $ Map.map (walk c) fs
                     walk _ TyString = TyString
                     walk _ TyUnit = TyUnit
                     walk _ TyFloat = TyFloat
                     walk _ TyNat = TyNat
                     walk _ TyKeyword = TyKeyword

--let typeShiftAbove d c tyT =
--  tymap
--    (fun c x n -> if x>=c then TyVar(x+d,n+d) else TyVar(x,n+d))
--    c tyT

typeShiftAbove :: Depth -> VarName -> Type -> Type
typeShiftAbove d c ty = typeMap onVar c ty
                  where onVar c name depth | name >= c = TyVar (name + d) (depth + d)
                        onVar c name depth = TyVar name (depth + d)

typeShift :: VarName -> Type -> Type
typeShift d ty = typeShiftAbove d 0 ty

--let typeSubst tyS j tyT =
--  tymap
--    (fun j x n -> if x=j then (typeShift j tyS) else (TyVar(x,n)))
--    j tyT

typeSubstitution :: Type -> VarName -> Type -> Type
typeSubstitution tyS j tyT = typeMap onVar j tyT
                    where onVar j x n | x == j = typeShift j tyS
                          onVar j x n = TyVar x n

--let typeSubstTop tyS tyT =
--  typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

typeSubstitutionTop :: Type -> Type -> Type
typeSubstitutionTop tyS tyT = typeShift (-1) (typeSubstitution (typeShift 1 tyS) 0 tyT)
