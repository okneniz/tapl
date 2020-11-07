module Language.TAPL.FullRef.Context where

import Data.Maybe (isJust)
import Control.Monad (when)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Context
import Language.TAPL.FullRef.Types

type LCNames = Names Binding
data LCState = LCState { names :: LCNames, memory :: LCMemory } deriving (Show)
type Eval a = StateT LCState (Except String) a

emptyState :: LCNames -> LCState
emptyState x = LCState { names = x, memory = [] }

getNames :: Eval LCNames
getNames = get >>= \s -> return $ names s

putNames :: LCNames -> Eval ()
putNames n = get >>= \s -> put $ s { names = n }

modifyNames :: (LCNames -> LCNames) -> Eval ()
modifyNames f = do
    m <- getNames
    putNames $ f m

getMemory :: Eval LCMemory
getMemory = get >>= \s -> return $ memory s

putMemory :: LCMemory -> Eval ()
putMemory m = get >>= \s -> put $ s { memory = m }

addName :: String -> LCNames -> LCNames
addName x = bind x NameBind

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty = bind x (VarBind ty)

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name <> "'")
pickFreshName c name = (name, c') where c' = addName name c

getBinding :: LCNames -> VarName -> Maybe Binding
getBinding ns varName =
    case bindingType ns varName of
         (Just binding) -> return $ bindingShift (varName + 1) binding
         x -> x

getTypeAbb :: LCNames -> VarName -> Maybe Type
getTypeAbb ns varName =
    case getBinding ns varName of
         (Just (TypeAddBind ty)) -> Just ty
         _ -> Nothing

isTypeAbb :: LCNames -> VarName -> Bool
isTypeAbb ns varName = isJust $ getTypeAbb ns varName

bindingShift :: VarName -> Binding -> Binding
bindingShift _ NameBind = NameBind
bindingShift d (VarBind ty) = VarBind (typeShift d ty)
bindingShift _ TypeVarBind = TypeVarBind
bindingShift d (TypeAddBind ty) = TypeAddBind (typeShift d ty)

type LCMemory = [Term]

extend :: Term -> Eval Location
extend t = do
    m <- getMemory
    putMemory $ m <> [t]
    return $ length m

deref :: Location -> Eval Term
deref l = do
    m <- getMemory
    when (l > length m) (lift $ throwE $ "Invalid memory address")
    when (l < 0)        (lift $ throwE $ "Invalid memory address")
    return $ m !! l

assign :: Location -> Term -> Eval ()
assign l t = getMemory >>= f l >>= putMemory
       where f 0 (_:rest) = return $ t:rest
             f i (x:rest) = f (i - 1) rest >>= \rest' -> return $ x:rest'
             f _ _ = lift $ throwE "invalid location"

shiftStore :: VarName -> Eval ()
shiftStore i = getMemory >>= \m -> putMemory $ termShift i <$> m
