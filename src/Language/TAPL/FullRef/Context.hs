module Language.TAPL.FullRef.Context where

import Language.TAPL.FullRef.Types
import Data.Maybe (isJust)
import Control.Monad (liftM)
import Data.List (intercalate)
import Control.Monad (when)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.FullRef.Types

type LCNames = [(String,Binding)]

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

type LCMemory = [Term]

extend :: Term -> Eval Location
extend t = do
    m <- getMemory
    putMemory $ m ++ [t]
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
             f l (x:rest) = f (l - 1) rest >>= \rest' -> return $ x:rest'
             f _ _ = lift $ throwE $ "invalid location"

shiftStore :: VarName -> Eval ()
shiftStore i = getMemory >>= \m -> putMemory $ termShift i <$> m
