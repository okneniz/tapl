module Language.TAPL.Bot.Context where

import Data.Maybe (isJust)
import Data.List (findIndex)
import Control.Monad (liftM)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Bot.Types

type Eval a = StateT LCNames (Except String) a
type LCNames = [(String,Binding)]

bind :: String -> Binding -> LCNames -> LCNames
bind x b n = (x,b):n

addName :: String -> LCNames -> LCNames
addName x n = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

isBound :: LCNames -> String -> Bool
isBound n name = isJust $ Prelude.lookup name n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName n x | isBound n x = pickFreshName n (x ++ "'")
pickFreshName n x = (x, n') where n' = addName x n

pickVar :: LCNames -> VarName -> Maybe (String, Binding)
pickVar [] _ = Nothing
pickVar names varname | length names > varname = Just $ names !! varname
pickVar _ _ = Nothing

nameFromContext :: LCNames -> VarName -> Maybe String
nameFromContext n v = liftM fst $ pickVar n v

findName names varName = liftM fst $ pickVar names varName

findVarName names name = findIndex ((== name) . fst) names

bindingType names varName = liftM snd $ pickVar names varName

getBinding names varName = bindingType names varName
