module Language.TAPL.FullUntyped.Context where

import Language.TAPL.FullUntyped.Types
import Data.Maybe (isJust)
import Control.Monad (liftM)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

type LCNames = [(String,Binding)]
type Eval a = StateT LCNames (Except String) a

bind :: String -> Binding -> LCNames -> LCNames
bind x b n = (x,b):n

addName :: String -> LCNames -> LCNames
addName x = bind x NameBind

addVar :: String -> LCNames -> LCNames
addVar x n = bind x VarBind n

isBound :: String -> LCNames -> Bool
isBound k n = isJust $ Prelude.lookup k n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound name c = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName name c

pickVar :: LCNames -> VarName -> Maybe (String, Binding)
pickVar [] _ = Nothing
pickVar names varname | length names > varname = Just $ names !! varname
pickVar _ _ = Nothing

nameFromContext :: LCNames -> VarName -> Maybe String
nameFromContext n v = liftM fst $ pickVar n v
