{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.TAPL.SimpleBool.Context where

import Language.TAPL.SimpleBool.Types
import Data.List (intercalate)
import Data.Maybe (isJust)
import Control.Monad (liftM)

data SimpleBoolContext t = SimpleBoolContext LCNames t

class LCContext c where
  bind :: c -> String -> Binding -> c
  addName :: c -> String -> c
  isBound :: c -> String -> Bool
  pickFreshName :: c -> String -> (String, c)
  pickVar :: c -> VarName -> Maybe (String, Binding)
  nameFromContext :: c -> VarName -> Maybe String
  names :: c -> LCNames
  withTerm :: c -> Term -> c

instance LCContext (SimpleBoolContext Term) where
  bind (SimpleBoolContext n t) x b = SimpleBoolContext ((x,b):n) t
  addName c x = bind c x NameBind
  isBound (SimpleBoolContext n _) name = isJust $ Prelude.lookup name n

  pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
  pickFreshName c name = (name, c') where c' = addName c name

  pickVar (SimpleBoolContext [] _) varname = Nothing
  pickVar (SimpleBoolContext names _) varname | length names > varname = Just $ names !! varname
  pickVar _ _ = Nothing

  nameFromContext c varname = liftM fst $ pickVar c varname
  names (SimpleBoolContext n _) = n
  withTerm (SimpleBoolContext ns _) t = SimpleBoolContext ns t

instance Show (SimpleBoolContext Term) where
  show (SimpleBoolContext _ (TTrue _)) = "true"
  show (SimpleBoolContext _ (TFalse _)) = "false"

  show c@(SimpleBoolContext _ (TIf _ t1 t2 t3)) =
    "if " ++ (show $ c `withTerm `t1) ++
    " then " ++ (show $ c `withTerm `t2) ++
    " else " ++ (show $ c `withTerm `t3)

  show c@(SimpleBoolContext _ v@(TVar _ varname depth)) =
    case nameFromContext c varname of
         Just name -> name
         _ -> "[bad index in " ++ show v ++ " in context " ++ show c  ++ "]"

  show c@(SimpleBoolContext n (TAbs _ name _ t)) =
      let (name', c') = pickFreshName (SimpleBoolContext n t) name
      in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

  show c@(SimpleBoolContext _ (TApp _ t1 t2)) =
    (show $ c `withTerm` t1) ++ " " ++ (show $ c `withTerm` t2)
