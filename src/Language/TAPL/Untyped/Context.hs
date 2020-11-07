module Language.TAPL.Untyped.Context where

import Language.TAPL.Untyped.Types
import Language.TAPL.Common.Context

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

type LCNames = Names Binding
type Eval a = StateT LCNames (Except String) a

addName :: String -> LCNames -> LCNames
addName x = bind x NameBind

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name <> "'")
pickFreshName c name = (name, c') where c' = addName name c
