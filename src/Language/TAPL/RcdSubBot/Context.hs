module Language.TAPL.RcdSubBot.Context where

import Data.Maybe (isJust)
import Data.List (findIndex)
import Control.Monad (liftM)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.RcdSubBot.Types

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
pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName name c

pickVar :: LCNames -> VarName -> Maybe (String, Binding)
pickVar [] _ = Nothing
pickVar names varname | length names > varname = Just $ names !! varname
pickVar _ _ = Nothing

findVarName :: Eq a => [(a, b)] -> a -> Maybe Int
findVarName names name = findIndex ((== name) . fst) names

nameFromContext :: LCNames -> VarName -> Maybe String
nameFromContext n v = liftM fst $ pickVar n v

bindingType :: LCNames -> VarName -> Maybe Binding
bindingType names varName = liftM snd $ pickVar names varName

getBinding :: LCNames -> VarName -> Maybe Binding
getBinding names varName = bindingType names varName

isTypeAbb :: LCNames -> VarName -> Bool
isTypeAbb names varName = isJust $ getTypeAbb names varName

getTypeAbb :: LCNames -> VarName -> Maybe Type
getTypeAbb names varName =
    case getBinding names varName of
         (Just (TypeAddBind ty)) -> Just ty
         _ -> Nothing

withTmpStateT :: Monad m => (s -> s) -> StateT s m b -> StateT s m b
withTmpStateT f g = do
    s <- get
    modify f
    x <- g
    put s
    return x