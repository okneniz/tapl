module Language.TAPL.TypedArith.Types where

import Language.TAPL.Common.Context
import Text.Parsec (SourcePos)

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          | TZero SourcePos
          | TSucc SourcePos Term
          | TPred SourcePos Term
          | TIsZero SourcePos Term
          deriving (Show)

data Type = TyBool
          | TyArrow Type Type
          | TyNat
          deriving (Eq)

type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)

instance Show Type where
    show (TyArrow t1 t2) = "("<> show t1 <> " -> " <> show t2 <> ")"
    show TyBool = "Bool"
    show TyNat = "Nat"

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal x | isNumerical x = True
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
                  walk _ (TZero p) = TZero p
                  walk c (TIsZero p t1) = TIsZero p (walk c t1)
                  walk c (TPred p t1) = TPred p (walk c t1)
                  walk c (TSucc p t1) = TSucc p (walk c t1)

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