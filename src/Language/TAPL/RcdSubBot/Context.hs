module Language.TAPL.RcdSubBot.Context where

import Data.Maybe (isJust)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.RcdSubBot.Types
import Language.TAPL.Common.Context

type LCNames = Names Binding
type Eval a = StateT LCNames (Except String) a

addName :: String -> LCNames -> LCNames
addName x n = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name <> "'")
pickFreshName c name = (name, c') where c' = addName name c

getBinding :: LCNames -> VarName -> Maybe Binding
getBinding names varName = bindingType names varName

getTypeAbb :: LCNames -> VarName -> Maybe Type
getTypeAbb names varName =
    case getBinding names varName of
         (Just (TypeAddBind ty)) -> Just ty
         _ -> Nothing

isTypeAbb :: LCNames -> VarName -> Bool
isTypeAbb names varName = isJust $ getTypeAbb names varName
