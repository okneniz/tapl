module Language.TAPL.Bot.Types where

import Text.Parsec (SourcePos)
import Language.TAPL.Common.Context

data Command = Eval [Term]
             | Bind SourcePos String Binding
             deriving (Show)

data Term = TVar SourcePos VarName Depth
          | TAbs SourcePos String Type Term
          | TApp SourcePos Term Term
          deriving (Show)

type AST = [Term]

data Binding = NameBind
             | VarBind Type
             | TypeVarBind
             | TypeAddBind Type
             deriving (Show)

data Type = TyArrow Type Type
          | TyTop
          | TyBot
          deriving (Show)

instance Eq Type where
  TyTop == TyTop = True
  TyBot == TyBot = True
  (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)
  _ == _ = False

(<:) :: Type -> Type -> Bool
_ <: TyTop = True
TyBot <: _ = True
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
x <: y | x == y = True
_ <: _ = False

isVal :: Term -> Bool
isVal (TAbs _ _ _ _) = True
isVal _ = False

tmmap :: (Int -> SourcePos -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar pos name depth) = onvar c pos name depth
                  walk c (TAbs pos x ty t1) = TAbs pos x ty (walk (c+1) t1)
                  walk c (TApp pos t1 t2) = TApp pos (walk c t1) (walk c t2)

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
