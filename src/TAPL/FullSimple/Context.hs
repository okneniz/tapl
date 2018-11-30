{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullSimple.Context where

import TAPL.FullSimple.Types
import Data.List (intercalate)
import Data.Maybe (isJust)
import Control.Monad (liftM)

data FullSimpleContext t = FullSimpleContext LCNames t

class LCContext c where
  bind :: c -> String -> Binding -> c
  addName :: c -> String -> c
  isBound :: c -> String -> Bool
  pickFreshName :: c -> String -> (String, c)
  pickVar :: c -> VarName -> Maybe (String, Binding)
  nameFromContext :: c -> VarName -> Maybe String
  names :: c -> LCNames
  withTerm :: c -> Term -> c

instance LCContext (FullSimpleContext Term) where
  bind (FullSimpleContext n t) x b = FullSimpleContext ((x,b):n) t
  addName c x = bind c x NameBind
  isBound (FullSimpleContext n _) name = isJust $ Prelude.lookup name n

  pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
  pickFreshName c name = (name, c') where c' = addName c name

  pickVar (FullSimpleContext [] _) varname = Nothing
  pickVar (FullSimpleContext names _) varname | length names > varname = Just $ names !! varname
  pickVar _ _ = Nothing

  nameFromContext c varname = liftM fst $ pickVar c varname
  names (FullSimpleContext n _) = n
  withTerm (FullSimpleContext ns _) t = FullSimpleContext ns t

instance Show (FullSimpleContext Term) where
  show (FullSimpleContext _ (TTrue _)) = "true"
  show (FullSimpleContext _ (TFalse _)) = "false"
  show (FullSimpleContext _ (TString _ s)) = show s
  show (FullSimpleContext _ (TUnit _)) = "unit"
  show (FullSimpleContext _ (TZero _)) = "zero"
  show (FullSimpleContext _ (TFloat _ t)) = show t
  show (FullSimpleContext _ (TInt _ x)) = show x
  show (FullSimpleContext _ (TKeyword _ x)) = x
  show c@(FullSimpleContext _ (TSucc _ t)) = "succ " ++ (show $ c `withTerm` t)
  show c@(FullSimpleContext _ (TPred _ t)) = "pred " ++ (show $ c `withTerm` t)
  show c@(FullSimpleContext _ (TIsZero _ t)) = "zero? " ++ (show $ c `withTerm` t)

  show c@(FullSimpleContext _ (TIf _ t1 t2 t3)) =
    "if " ++ (show $ c `withTerm `t1) ++
    " then " ++ (show $ c `withTerm `t2) ++
    " else " ++ (show $ c `withTerm `t3)

  show c@(FullSimpleContext _ v@(TVar _ varname depth)) =
    case nameFromContext c varname of
         Just name -> name
         _ -> "[bad index in " ++ show v ++ " in context " ++ show c  ++ "]"

  show c@(FullSimpleContext n (TAbs _ name _ t)) =
      let (name', c') = pickFreshName (FullSimpleContext n t) name
      in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

  show c@(FullSimpleContext _ (TApp _ t1 t2)) =
    (show $ c `withTerm` t1) ++ " " ++ (show $ c `withTerm` t2)

  show c@(FullSimpleContext _ (TPair _ t1 t2)) =
    "{" ++ (show $ c `withTerm` t1) ++ "," ++ (show $ c `withTerm` t2) ++ "}"

  show c@(FullSimpleContext n (TRecord _ ts)) =
      "{" ++ (intercalate ", " $ (\(k,ty) -> k ++ "=" ++ (show $ c `withTerm` ty)) <$> ts) ++ "}"

  show c@(FullSimpleContext n (TLookup _ t k)) =
    (show $ c `withTerm` t) ++ "." ++ (show $ c `withTerm` k)

  show (FullSimpleContext n (TLet _ v t1 t2)) =
      "let " ++ v ++ " = " ++ show (FullSimpleContext n t1) ++ " in " ++ show (FullSimpleContext n t2)

  show (FullSimpleContext ns (TTag _ key t ty)) =
    "<" ++ key ++ "=" ++ show (FullSimpleContext ns t) ++ ">"

  show c@(FullSimpleContext _ (TAscribe _ t ty)) =
    (show $ c `withTerm` t) ++ ":" ++ show ty

  show c@(FullSimpleContext ns (TCase _ t cases)) =
    "case " ++ (show $ c `withTerm` t) ++ " of " ++
    intercalate " | " (df <$> cases)
    where df (caseName, varName, t) = "<" ++ caseName ++ "=" ++ varName ++ "> -> " ++ show (FullSimpleContext ns t)

  show (FullSimpleContext ns (TFix _ t)) = show (FullSimpleContext ns t)
