module Language.TAPL.Common.Context where

import Data.List (findIndex)
import Data.Maybe (isJust)

type Names a = [(String,a)]
type VarName = Int
type Depth = Int

bind :: String -> a -> Names a -> Names a
bind x b n = (x,b):n

isBound :: Names a -> String -> Bool
isBound n name = isJust $ lookup name n

pickVar :: Names a -> VarName -> Maybe (String, a)
pickVar [] _ = Nothing
pickVar names varname | length names > varname = Just $ names !! varname
pickVar _ _ = Nothing

nameFromContext :: Names a -> VarName -> Maybe String
nameFromContext n v = fst <$> pickVar n v

bindingType :: Names a -> VarName -> Maybe a
bindingType names varName = snd <$> pickVar names varName

findVarName :: Names a -> String -> Maybe VarName
findVarName names name = findIndex ((== name) . fst) names

findName :: Names a -> VarName -> Maybe String
findName names varName = fst <$> pickVar names varName