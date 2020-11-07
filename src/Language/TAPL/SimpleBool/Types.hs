module Language.TAPL.SimpleBool.Types where

import Text.Parsec (SourcePos)

data Term = TTrue SourcePos
          | TFalse SourcePos
          | TIf SourcePos Term Term Term
          | TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          deriving (Show)

data Type = TyBool
          | TyArrow Type Type
          deriving (Eq)

type VarName = Int
type Depth = Int
type Location = Int
type AST = [Term]

data Binding = NameBind | VarBind Type deriving (Show)

instance Show Type where
    show (TyArrow t1 t2) = "("<> show t1 <> " -> " <> show t2 <> ")"
    show TyBool = "Bool"

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal _ = False

tmmap :: (Int -> SourcePos -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar pos name depth) = onvar c pos name depth
                  walk c (TAbs pos x ty t1) = TAbs pos x ty (walk (c+1) t1)
                  walk c (TApp pos t1 t2) = TApp pos (walk c t1) (walk c t2)
                  walk c (TIf pos t1 t2 t3) = TIf pos (walk c t1) (walk c t2) (walk c t3)
                  walk _ (TTrue pos) = TTrue pos
                  walk _ (TFalse pos) = TFalse pos

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = tmmap onvar s t
                 where onvar c pos name depth | name >= c = TVar pos (name + d) (depth + d)
                       onvar _ pos name depth = TVar pos name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c _ name _ | name == j + c = shift c s
                     onvar _ pos name depth = TVar pos name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
