module Language.TAPL.TypedArith.Context where

import Language.TAPL.TypedArith.Types
import Language.TAPL.Common.Context

type LCNames = Names Binding

addName :: LCNames -> String -> LCNames
addName n x = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName c name
