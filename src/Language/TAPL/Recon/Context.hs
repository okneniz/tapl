module Language.TAPL.Recon.Context where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.Recon.Types
import Language.TAPL.Common.Context

type LCNames = Names Binding
data LCState = LCState { names :: LCNames, varIndex :: VarIndex, constraints :: [Constraint] } deriving (Show)
type Eval a = StateT LCState (Except String) a

newState :: LCNames -> LCState
newState x = LCState { names = x, varIndex = 0, constraints = [] }

getNames :: Eval LCNames
getNames = names <$> get

getConstraints :: Eval [Constraint]
getConstraints = constraints <$> get

putConstraints :: [Constraint] -> Eval ()
putConstraints x = get >>= \s -> put $ s { constraints = x }

putVar :: String -> Type -> LCState -> LCState
putVar x ty s = s { names = (bind x (VarBind ty) (names s)) }

addName :: String -> LCNames -> LCNames
addName x = bind x NameBind

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty = bind x (VarBind ty)

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name <> "'")
pickFreshName c name = (name, c') where c' = addName name c
