module Language.TAPL.FullFSub.Context where

import Data.Maybe (isJust)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Context
import Language.TAPL.FullFSub.Types

type LCNames = Names Binding
type Eval a = StateT LCNames (Except String) a

addName :: String -> LCNames -> LCNames
addName x = bind x NameBind

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty = bind x (VarBind ty)

addTypeVar :: String -> Type -> LCNames -> LCNames
addTypeVar x ty n = bind x (TypeVarBind ty) n

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
bindingShift d (TypeVarBind ty) = TypeVarBind (typeShift d ty)
bindingShift d (TypeAddBind ty) = TypeAddBind (typeShift d ty)
