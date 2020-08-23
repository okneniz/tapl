module Language.TAPL.Untyped.Types where

import Text.Parsec (SourcePos)
import Language.TAPL.Common.Context (VarName, Depth)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Term
          | TApp SourcePos Term Term
          deriving (Show)

data Binding = NameBind deriving (Show)

isVal :: Term -> Bool
isVal (TAbs _ _ _) = True
isVal _ = False

tmmap :: (Int -> SourcePos -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar p name depth) = onvar c p name depth
                  walk c (TAbs p x t1) = TAbs p x (walk (c+1) t1)
                  walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)

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
