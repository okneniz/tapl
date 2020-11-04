module Language.TAPL.FullUntyped.Types where

import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Text.Parsec (SourcePos)

data Command = Eval [Term] deriving (Show)

data Term = TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TVar SourcePos VarName Depth
          | TAbs SourcePos String Term
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
          | TProj SourcePos Term String
          | TLet SourcePos String Term Term
          | TTimesFloat SourcePos Term Term
          deriving (Show)

type VarName = Int
type Depth = Int
type Location = Int
type AST = [Term]

data Binding = NameBind | VarBind deriving (Show)

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _) = True
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TFloat _ _) = True
isVal x | isNumerical x = True
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

tmmap :: (Int -> SourcePos -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar p name depth) = onvar c p name depth
                  walk c (TAbs p x t1) = TAbs p x (walk (c+1) t1)
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
                  walk c (TPair p t1 t2) = TPair p (walk c t1) (walk c t2)
                  walk c (TRecord p fields) = TRecord p $ Map.map (walk c) fields
                  walk c (TProj p r k) = TProj p (walk c r) k
                  walk c (TLet p x t1 t2) = TLet p x (walk c t1) (walk (c+1) t2)
                  walk c (TTimesFloat p t1 t2) = TTimesFloat p (walk c t1) (walk c t2)

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
