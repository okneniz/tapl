{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module TAPL.Untyped.Context where

import TAPL.Untyped.Types
import Data.List (intercalate)
import Data.Maybe (isJust)
import Control.Monad (liftM)

data UntypedContext t = UntypedContext LCNames t

class LCContext c where
  bind :: c -> String -> Binding -> c
  addName :: c -> String -> c
  isBound :: c -> String -> Bool
  pickFreshName :: c -> String -> (String, c)
  pickVar :: c -> VarName -> Maybe (String, Binding)
  nameFromContext :: c -> VarName -> Maybe String
  names :: c -> LCNames
  withTerm :: c -> Term -> c

instance LCContext (UntypedContext Term) where
  bind (UntypedContext n t) x b = UntypedContext ((x,b):n) t
  addName c x = bind c x NameBind
  isBound (UntypedContext n _) name = isJust $ Prelude.lookup name n

  pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
  pickFreshName c name = (name, c') where c' = addName c name

  pickVar (UntypedContext [] _) varname = Nothing
  pickVar (UntypedContext names _) varname | length names > varname = Just $ names !! varname
  pickVar _ _ = Nothing

  nameFromContext c varname = liftM fst $ pickVar c varname
  names (UntypedContext n _) = n
  withTerm (UntypedContext ns _) t = UntypedContext ns t

instance Show (UntypedContext Term) where
  show c@(UntypedContext _ v@(TVar _ varname depth)) =
    case nameFromContext c varname of
         Just name -> name
         _ -> "[bad index in " ++ show v ++ " in context " ++ show c  ++ "]"

  show c@(UntypedContext n (TAbs _ name t)) =
      let (name', c') = pickFreshName (UntypedContext n t) name
      in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

  show c@(UntypedContext _ (TApp _ t1 t2)) =
    (show $ c `withTerm` t1) ++ " " ++ (show $ c `withTerm` t2)
