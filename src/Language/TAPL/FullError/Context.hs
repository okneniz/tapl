module Language.TAPL.FullError.Context where

import Data.Maybe (isJust)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Context
import Language.TAPL.FullError.Types

type Eval a = StateT LCNames (Except String) a
type LCNames = [(String,Binding)]

addName :: String -> LCNames -> LCNames
addName x n = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName n x | isBound n x = pickFreshName n (x <> "'")
pickFreshName n x = (x, n') where n' = addName x n

getBinding :: LCNames -> VarName -> Maybe Binding
getBinding names varName =
    case bindingType names varName of
         (Just binding) -> return $ bindingShift (varName + 1) binding
         x -> x

getTypeAbb :: LCNames -> VarName -> Maybe Type
getTypeAbb names varName =
    case getBinding names varName of
         (Just (TypeAddBind ty)) -> Just ty
         _ -> Nothing

isTypeAbb :: LCNames -> VarName -> Bool
isTypeAbb names varName = isJust $ getTypeAbb names varName

bindingShift :: VarName -> Binding -> Binding
bindingShift _ NameBind = NameBind
bindingShift d (VarBind ty) = VarBind (typeShift d ty)
bindingShift _ TypeVarBind = TypeVarBind
bindingShift d (TypeAddBind ty) = TypeAddBind (typeShift d ty)
