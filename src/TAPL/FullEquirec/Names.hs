{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullEquirec.Names where

import TAPL.FullEquirec.Types
import Data.List (intercalate, findIndex, find, sortBy)
import Data.Maybe (isJust)
import Data.Either
import Control.Monad (liftM)
import Data.Function (on)

class LCNames c where
    pickFreshName :: c -> String -> (String, c)
    bind :: c -> String -> Binding -> Names
    addName :: c -> String -> Names
    findVarName :: c -> String -> Maybe VarName
    pickVar :: c -> VarName -> Maybe (String, Binding)
    findName :: c -> VarName -> Maybe String
    bindingType :: c -> VarName -> Maybe Binding
    addVar :: c -> String -> Type -> Names
    addTyVar :: c -> String -> Names
    isBound :: c -> String -> Bool
    depth :: c -> Depth
    getBinding :: c -> VarName -> Maybe Binding
    getTypeAbb :: c -> VarName -> Maybe Type
    isTypeAbb :: c -> VarName -> Bool

type Names = [(String,Binding)]

instance LCNames Names where
    pickFreshName names name | isBound names name = pickFreshName names (name ++ "'")
    pickFreshName names name = (name, addName names name)
    bind names name binding = (name,binding):names

    pickVar [] varName = Nothing
    pickVar names varName | length names > varName = Just $ names !! varName
    pickVar _ _ = Nothing

    findName names varName = liftM fst $ pickVar names varName
    findVarName names name = findIndex ((== name) . fst) names

    bindingType names varName = liftM snd $ pickVar names varName

    addName  names name    = bind names name NameBind
    addVar   names name ty = bind names name (VarBind ty)

    isBound names name = isJust $ Prelude.lookup name names
    depth = length

    getBinding names varName = case bindingType names varName of
                                    (Just binding) -> return $ bindingShift (varName + 1) binding
                                    x -> x

    isTypeAbb names varName = isJust $ getTypeAbb names varName

    getTypeAbb names varName =
        case getBinding names varName of
             (Just (TypeAddBind ty)) -> Just ty
             x -> Nothing
