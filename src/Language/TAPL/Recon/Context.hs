module Language.TAPL.Recon.Context where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import Language.TAPL.Recon.Types
import Language.TAPL.Common.Context

type LCNames = Names Binding
type Eval a = ReaderT LCNames (ExceptT String (State (VarIndex, [Constraint]))) a

addName :: String -> LCNames -> LCNames
addName x n = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName name c
