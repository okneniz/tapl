{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPL.Recon.Context where

import TAPL.Recon.Types
import Data.List (findIndex)
import Data.Maybe
import Control.Monad (liftM)

data UVar = UVar String UVarGen
type UVarGen = () -> UVar
type Constraint = (Type, Type)
type Names = [(String,Binding)]

data Context t = Context Names UVarGen [Constraint] t

data EvaluationError = WrongKindOfBinding Info VarName
                     | CircularConstrains [Constraint]
                     | UnresolvedConstraints [Constraint]
                     | AttemptToEvaluateEmptyAST

newUVarGen :: UVarGen
newUVarGen = f 0 where f n _ = UVar ("x" ++ show n) (f (n+1))

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
    varType :: c -> VarName -> Maybe Type

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

    varType names varName = case bindingType names varName of
                                 Just (VarBind ty) -> Just ty
                                 _ -> Nothing

