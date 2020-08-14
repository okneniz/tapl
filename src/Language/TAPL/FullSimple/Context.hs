module Language.TAPL.FullSimple.Context where

import Language.TAPL.FullSimple.Types
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

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty n = bind x (VarBind ty) n

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

bindingType :: LCNames -> VarName -> Maybe Binding
bindingType names varName = liftM snd $ pickVar names varName

getBinding names varName =
    case bindingType names varName of
         (Just binding) -> return $ bindingShift (varName + 1) binding
         x -> x

isTypeAbb names varName = isJust $ getTypeAbb names varName

getTypeAbb names varName =
    case getBinding names varName of
         (Just (TypeAddBind ty)) -> Just ty
         x -> Nothing

bindingShift :: VarName -> Binding -> Binding
bindingShift d NameBind = NameBind
bindingShift d (VarBind ty) = VarBind (typeShift d ty)
bindingShift d TypeVarBind = TypeVarBind
bindingShift d (TypeAddBind ty) = TypeAddBind (typeShift d ty)

withTmpStateT f g = do
    s <- get
    modify f
    x <- g
    put s
    return x
    return x

computeType :: Type -> Eval (Maybe Type)
computeType (TyVar i _) = do
    n <- get
    if isTypeAbb n i
    then return $ getTypeAbb n i
    else return Nothing

computeType _ = return Nothing

simplifyType :: Type -> Eval Type
simplifyType ty = do
    n <- computeType ty
    case n of
         Just x -> simplifyType x
         _ -> return ty
