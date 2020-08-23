module Language.TAPL.Untyped.Context where

import Language.TAPL.Untyped.Types
import Language.TAPL.Common.Context

type LCNames = Names Binding

addName :: String -> LCNames -> LCNames
addName x n = bind x NameBind n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName name c
