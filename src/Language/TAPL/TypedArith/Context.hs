module Language.TAPL.TypedArith.Context where

import Language.TAPL.TypedArith.Types
import Language.TAPL.Common.Context

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

type LCNames = [(String,Binding)]
type Eval a = StateT LCNames (Except String) a

addName :: String -> LCNames -> LCNames
addName x n = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName name c
