module Language.TAPL.FullSimple.Types where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Command = Eval [Term]
             | Bind Info String Binding
             deriving (Show)

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
          | TRecord Info (Map String Term)
          | TLookup Info Term Term
          | TLet Info String Term Term
          | TAscribe Info Term Type
          | TCase Info Term (Map String (String, Term))
          | TTag Info String Term Type
          | TKeyword Info String
          | TFix Info Term
          deriving (Show)

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
          | TyTop
          | TyBot
          deriving (Show)

type VarName = Int
type Depth = Int
type Location = Int
type AST = [Term]

data Info = Info { row :: Int, column :: Int } deriving (Show)

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
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal x | isNumerical x = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TAscribe _ t _) = isVal t
isVal (TTag _ _ t _) = isVal t
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
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
                  walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                  walk c (TRecord info fields) = TRecord info $ Map.map (walk c) fields
                  walk c (TTag info k t1 ty) = TTag info k (walk c t1) ty
                  walk c (TLookup info r k) = TLookup info (walk c r) k
                  walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)
                  walk c (TAscribe info t1 ty) = TAscribe info (walk c t1) ty
                  walk c (TCase info t1 branches) = TCase info (walk c t1) $ Map.map walkBranch branches
                                              where walkBranch (x, y) = (x, walk (c + 1) y)
                  walk c (TFix info t1) = TFix info (walk c t1)
                  walk _ t1@(TKeyword _ _) = t1

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = tmmap onvar s t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar _ info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c _ name _ | name == j + c = shift c s
                     onvar _ info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
