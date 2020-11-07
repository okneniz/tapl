module Language.TAPL.SimpleBool.Context where

import Language.TAPL.SimpleBool.Types
import Language.TAPL.Common.Context

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

type LCNames = Names Binding
type Eval a = StateT LCNames (Except String) a

addName :: LCNames -> String -> LCNames
addName n x = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty = bind x (VarBind ty)

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name <> "'")
pickFreshName c name = (name, c') where c' = addName c name
