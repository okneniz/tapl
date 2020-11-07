module Language.TAPL.TypedArith.Context where

import Language.TAPL.TypedArith.Types
import Language.TAPL.Common.Context

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

type LCNames = [(String,Binding)]
type Eval a = StateT LCNames (Except String) a

addName :: String -> LCNames -> LCNames
addName x = bind x NameBind

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty = bind x (VarBind ty)

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name <> "'")
pickFreshName c name = (name, c') where c' = addName name c
