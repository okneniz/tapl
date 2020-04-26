module Language.TAPL.Recon.Context where

import Data.Maybe
import Control.Monad (liftM)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy

import Language.TAPL.Recon.Types

type LCNames = [(String,Binding)]

type Eval a = ReaderT LCNames (ExceptT String (State (VarIndex, [Constraint]))) a

bind :: String -> Binding -> LCNames -> LCNames
bind x b n = (x,b):n

addName :: LCNames -> String -> LCNames
addName n x = bind x NameBind n

isBound :: LCNames -> String -> Bool
isBound n name = isJust $ Prelude.lookup name n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName c name

pickVar :: LCNames -> VarName -> Maybe (String, Binding)
pickVar [] _ = Nothing
pickVar names varname | length names > varname = Just $ names !! varname
pickVar _ _ = Nothing

nameFromContext :: LCNames -> VarName -> Maybe String
nameFromContext n v = liftM fst $ pickVar n v
