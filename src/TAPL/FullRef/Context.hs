{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullRef.Context where

import TAPL.FullRef.Types
import Data.Maybe (isJust)
import Control.Monad (liftM)
import Data.List (intercalate)

data FullRefContext t = FullRefContext LCNames LCMemory t

class LCContext c where
  bind :: c -> String -> Binding -> c
  addName :: c -> String -> c
  isBound :: c -> String -> Bool
  pickFreshName :: c -> String -> (String, c)
  pickVar :: c -> VarName -> Maybe (String, Binding)
  nameFromContext :: c -> VarName -> Maybe String
  names :: c -> LCNames
  withTerm :: c -> Term -> c

instance LCContext (FullRefContext Term) where
  bind (FullRefContext n m t) x b = FullRefContext ((x,b):n) m t
  addName c x = bind c x NameBind
  isBound (FullRefContext n m _) name = isJust $ Prelude.lookup name n

  pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
  pickFreshName c name = (name, c') where c' = addName c name

  pickVar (FullRefContext [] m _) varname = Nothing
  pickVar (FullRefContext names m _) varname | length names > varname = Just $ names !! varname
  pickVar _ _ = Nothing

  nameFromContext c varname = liftM fst $ pickVar c varname
  names (FullRefContext n _ _) = n
  withTerm (FullRefContext ns m _) t = FullRefContext ns m t

instance Show (FullRefContext Term) where
  show (FullRefContext _ _ (TTrue _)) = "true"
  show (FullRefContext _ _ (TFalse _)) = "false"
  show (FullRefContext _ _ (TString _ x)) = show x
  show (FullRefContext _ _ (TFloat _ x)) = show x
  show (FullRefContext _ _ (TInt _ x)) = show x
  show (FullRefContext _ _ (TKeyword _ x)) = x
  show (FullRefContext _ _ (TUnit _)) = "unit"
  show (FullRefContext _ _ (TZero _)) = "zero"
  show (FullRefContext n s (TSucc _ t)) = "succ " ++ show (FullRefContext n s t)
  show (FullRefContext n s (TPred _ t)) = "pred " ++ show (FullRefContext n s t)
  show (FullRefContext n s (TIsZero _ t)) = "zero? " ++ show (FullRefContext n s t)

  show (FullRefContext n s (TIf _ t1 t2 t3)) =
      "if " ++ show (FullRefContext n s t1) ++
      " then " ++ show (FullRefContext n s t2) ++
      " else " ++ show (FullRefContext n s t3)

  show c@(FullRefContext n s (TVar _ varname depth)) =
      case nameFromContext c varname of
           Just s -> s
           _ -> "[bad index in " ++ show varname ++ " in context " ++ show n  ++ "]"

  show c@(FullRefContext n s (TAbs _ name _ t)) =
      let (name', c') = pickFreshName (FullRefContext n s t) name
      in "(lambda " ++ name' ++ "." ++ show c'  ++ ")"

  show (FullRefContext n s (TApp _ t1 t2)) = show (FullRefContext n s t1) ++ " " ++ show (FullRefContext n s t2)
  show (FullRefContext n s (TRef _ t)) = "ref " ++ show (FullRefContext n s t)
  show (FullRefContext n s (TDeref _ t)) = "!" ++ show (FullRefContext n s t)
  show (FullRefContext n s (TAssign _ t1 t2)) = show (FullRefContext n s t1) ++ " := " ++ show (FullRefContext n s t2)
  show (FullRefContext n s (TLoc _ pointer)) = "<" ++ show pointer ++ ">"

  show (FullRefContext n s (TLet _ v t1 t2)) =
      "let " ++ v ++ " = " ++ show (FullRefContext n s t1) ++ " in " ++ show (FullRefContext n s t2)

  show c@(FullRefContext n s (TPair _ t1 t2)) =
      "{" ++ (show $ c `withTerm` t1) ++ "," ++ (show $ c `withTerm` t2) ++ "}"

  show c@(FullRefContext n s (TRecord _ ts)) =
      "{" ++ (intercalate ", " $ (\(k,ty) -> k ++ "=" ++ (show $ c `withTerm` ty)) <$> ts) ++ "}"

  show c@(FullRefContext n s (TLookup _ t k)) =
      (show $ c `withTerm` t) ++ "." ++ (show $ c `withTerm` k)

  show c@(FullRefContext n s (TAscribe _ t ty)) =
    (show $ c `withTerm` t) ++ ":" ++ show ty

  show c@(FullRefContext n s (TFix _ t)) = show $ c `withTerm` t

  show (FullRefContext ns s (TTag _ key t ty)) =
    "<" ++ key ++ "=" ++ show (FullRefContext ns s t) ++ ">"

  show c@(FullRefContext ns s (TCase _ t cases)) =
    "case " ++ (show $ c `withTerm` t) ++ " of " ++
    intercalate " | " (df <$> cases)
    where df (caseName, varName, t) = "<" ++ caseName ++ "=" ++ varName ++ "> -> " ++ show (FullRefContext ns s t)

instance Show (FullRefContext AST) where
  show (FullRefContext n m ast) = intercalate ";" $ (\t -> show $ FullRefContext n m t) <$> ast
