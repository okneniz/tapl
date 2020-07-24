module Language.TAPL.FullRef.Types where

import Text.Parsec (SourcePos)

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map

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
          deriving (Show)

data Type = TyBool
          | TyString
          | TyUnit
          | TyNat
          | TyFloat
          | TyInt
          | TyArrow Type Type
          | TyRef Type
          | TyID String
          | TyTop
          | TyBot
          | TyProduct Type Type
          | TyRecord (Map String Type)
          | TyKeyword
          | TyVariant (Map String Type)
          deriving (Show)

type VarName = Int
type Depth = Int
type AST = [Term]

instance Eq Type where
  TyBool == TyBool = True
  TyString == TyString = True
  TyUnit == TyUnit = True
  TyNat == TyNat = True
  TyFloat == TyFloat = True
  TyInt == TyInt = True
  (TyID x) == (TyID y) = x == y
  TyTop == TyTop = True
  TyBot == TyBot = True
  (TyRef tyT1) == (TyRef tyT2) = tyT1 == tyT2
  (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)
  (TyProduct tyT1 tyT2) == (TyProduct tyT1' tyT2') = (tyT1 == tyT2) && (tyT1' == tyT2')
  (TyRecord tys1) == (TyRecord tys2) = tys1 == tys2
  (TyVariant tys1) == (TyVariant tys2) = tys1 == tys2
  _ == _ = False

(<:) :: Type -> Type -> Bool
_ <: TyTop = True
TyBot <: _ = True
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
(TyRef tyT1) <: (TyRef tyT2) = (tyT1 <: tyT2) && (tyT2 <: tyT1)
(TyProduct tyS1 tyS2) <: (TyProduct tyT1 tyT2) = (tyS1 <: tyT1) && (tyS2 <: tyT2)

(TyRecord ty1) <: (TyRecord ty2) =
    all f $ Map.elems $ Map.intersectionWith (,) ty1 $ Map.filterWithKey (\k _ -> Map.member k ty1) ty2
    where f (ty1', ty2') = ty1' <: ty2'

(TyVariant ty1) <: (TyVariant ty2) =
    all f $ Map.elems $ Map.intersectionWith (,) ty1 $ Map.filterWithKey (\k _ -> Map.member k ty1) ty2
    where f (ty1', ty2') = ty1' <: ty2'

x <: y | x == y = True
_ <: _ = False

type LCMemory = [Term]
type Location = Int

extend :: LCMemory -> Term -> (Location, LCMemory)
extend s t = (size s, (s ++ [t]))

lookup :: LCMemory -> Location -> Term
lookup s l = s !! l

update :: LCMemory -> Location -> Term -> LCMemory
update s l t =
    let f 0 (_:rest) = t:rest
        f location (t':rest) = t':(f (location - 1) rest)
        f _ _ = error "invalid location"
    in f l s

size :: LCMemory -> Int
size = length


isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TString _ _) = True
isVal (TFloat _ _) = True
isVal (TInt _ _) = True
isVal (TUnit _) = True
isVal (TZero _) = True
isVal (TAscribe _ t _) = isVal t
isVal t | isNumerical t = True
isVal (TAbs _ _ _ _) = True
isVal (TLoc _ _) = True
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TFix _ t) = isVal t
isVal (TTag _ _ t _) = isVal t
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ t) = isNumerical t
isNumerical _ = False

termMap :: (Int -> SourcePos -> Depth -> VarName -> Term) -> Int -> Term -> Term
termMap onvar s t = walk s t
              where walk c (TVar info name depth) = onvar c info name depth
                    walk c (TAbs info x ty t1) = TAbs info x ty (walk (c+1) t1)
                    walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                    walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                    walk _ (TTrue info) = TTrue info
                    walk _ (TFalse info) = TFalse info
                    walk _ (TString info x) = TString info x
                    walk _ (TUnit info) = TUnit info
                    walk _ (TZero info) = TZero info
                    walk c (TIsZero info t1) = TIsZero info (walk c t1)
                    walk c (TPred info t1) = TPred info (walk c t1)
                    walk c (TSucc info t1) = TSucc info (walk c t1)
                    walk _ (TFloat info t1) = TFloat info t1
                    walk _ (TInt info t1) = TInt info t1
                    walk c (TAssign info t1 t2) = TAssign info (walk c t1) (walk c t2)
                    walk c (TRef info t1) = TRef info (walk c t1)
                    walk c (TDeref info t1) = TDeref info (walk c t1)
                    walk c (TLet info v t1 t2) = TLet info v (walk c t1) (walk (c + 1) t2)
                    walk c (TAscribe info t1 ty) = TAscribe info (walk c t1) ty
                    walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                    walk c (TRecord info fields) = TRecord info $ Map.map (walk c) fields
                    walk c (TProj info t1 t2) = TProj info (walk c t1) (walk c t2)
                    walk c (TTag info k t1 ty) = TTag info k (walk c t1) ty
                    walk c (TCase info t1 branches) = TCase info (walk c t1) $ Map.map walkBranch branches
                                                where walkBranch (x, y) = (x, walk (c + 1) y)
                    walk _ t1@(TKeyword _ _) = t1
                    walk c (TFix info t1) = TFix info (walk c t1)
                    walk _ t1@(TLoc _ _) = t1

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = termMap onvar s t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar _ info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = termMap onvar 0 t
               where onvar c _ name _ | name == j + c = shift c s
                     onvar _ info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
