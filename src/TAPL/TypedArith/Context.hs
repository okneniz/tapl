{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module TAPL.TypedArith.Context where

import TAPL.TypedArith.Types
import Data.List (intercalate)
import Data.Maybe (isJust)
import Control.Monad (liftM)

data TypedArithContext t = TypedArithContext LCNames t

class LCContext c where
  bind :: c -> String -> Binding -> c
  addName :: c -> String -> c
  isBound :: c -> String -> Bool
  pickFreshName :: c -> String -> (String, c)
  pickVar :: c -> VarName -> Maybe (String, Binding)
  nameFromContext :: c -> VarName -> Maybe String
  names :: c -> LCNames
  withTerm :: c -> Term -> c

instance LCContext (TypedArithContext Term) where
  bind (TypedArithContext n t) x b = TypedArithContext ((x,b):n) t
  addName c x = bind c x NameBind
  isBound (TypedArithContext n _) name = isJust $ Prelude.lookup name n

  pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
  pickFreshName c name = (name, c') where c' = addName c name

  pickVar (TypedArithContext [] _) varname = Nothing
  pickVar (TypedArithContext names _) varname | length names > varname = Just $ names !! varname
  pickVar _ _ = Nothing

  nameFromContext c varname = liftM fst $ pickVar c varname
  names (TypedArithContext n _) = n
  withTerm (TypedArithContext ns _) t = TypedArithContext ns t

instance Show (TypedArithContext Term) where
  show (TypedArithContext _ (TTrue _)) = "true"
  show (TypedArithContext _ (TFalse _)) = "false"
  show (TypedArithContext _ (TZero _)) = "zero"
  show c@(TypedArithContext _ (TSucc _ t)) = "succ " ++ (show $ c `withTerm` t)
  show c@(TypedArithContext _ (TPred _ t)) = "pred " ++ (show $ c `withTerm` t)
  show c@(TypedArithContext _ (TIsZero _ t)) = "zero? " ++ (show $ c `withTerm` t)

  show c@(TypedArithContext _ (TIf _ t1 t2 t3)) =
    "if " ++ (show $ c `withTerm `t1) ++
    " then " ++ (show $ c `withTerm `t2) ++
    " else " ++ (show $ c `withTerm `t3)

  show c@(TypedArithContext _ v@(TVar _ varname depth)) =
    case nameFromContext c varname of
         Just name -> name
         _ -> "[bad index in " ++ show v ++ " in context " ++ show c  ++ "]"

  show c@(TypedArithContext n (TAbs _ name _ t)) =
      let (name', c') = pickFreshName (TypedArithContext n t) name
      in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

  show c@(TypedArithContext _ (TApp _ t1 t2)) =
    (show $ c `withTerm` t1) ++ " " ++ (show $ c `withTerm` t2)
