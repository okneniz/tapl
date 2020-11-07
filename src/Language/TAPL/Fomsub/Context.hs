module Language.TAPL.Fomsub.Context where

import Data.Maybe (isJust)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Context
import Language.TAPL.Fomsub.Types

type LCNames = Names Binding
type Eval a = StateT LCNames (Except String) a

addName :: String -> LCNames -> LCNames
addName x n = bind x NameBind n

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

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

bindingShift :: VarName -> Binding -> Binding
bindingShift _ NameBind = NameBind
bindingShift d (VarBind ty) = VarBind (typeShift d ty)
bindingShift d (TypeVarBind ty) = TypeVarBind (typeShift d ty)

makeTop Star = TyTop
makeTop (Arrow k1 k2) = TyAbs "_" k1 $ makeTop k2
