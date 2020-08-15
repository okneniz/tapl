module Language.TAPL.FullRecon.Context where

import Data.Maybe
import Control.Monad (liftM)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.FullRecon.Types

type LCNames = [(String,Binding)]
data LCState = LCState { names :: LCNames, varIndex :: VarIndex, constraints :: [Constraint] } deriving (Show)
type Eval a = StateT LCState (Except String) a

newState :: LCNames -> LCState
newState x = LCState { names = x, varIndex = 0, constraints = [] }

getNames :: Eval LCNames
getNames = do
    state <- get
    return $ names state

putVar :: String -> Type -> LCState -> LCState
putVar x ty state = state { names = (bind x (VarBind ty) (names state)) }

bind :: String -> Binding -> LCNames -> LCNames
bind x b n = (x,b):n

addName :: LCNames -> String -> LCNames
addName n x = bind x NameBind n

isBound :: LCNames -> String -> Bool
isBound n name = isJust $ Prelude.lookup name n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name ++ "'")
pickFreshName c name = (name, c') where c' = addName c name

pickVar :: LCNames -> VarName -> Maybe (String, Binding)
pickVar [] _ = Nothing
pickVar names varname | length names > varname = Just $ names !! varname
pickVar _ _ = Nothing

nameFromContext :: LCNames -> VarName -> Maybe String
nameFromContext n v = liftM fst $ pickVar n v

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal x | isNumerical x = True
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> Int -> Term -> Term
termMap onVar s t = walk s t
              where walk c (TVar info name depth) = onVar c info name depth
                    walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
                    walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                    walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                    walk c (TTrue info) = TTrue info
                    walk c (TFalse info) = TFalse info
                    walk c (TZero info) = TZero info
                    walk c (TIsZero info t) = TIsZero info (walk c t)
                    walk c (TPred info t) = TPred info (walk c t)
                    walk c (TSucc info t) = TSucc info (walk c t)
                    walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onVar c t
                 where onVar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onVar c info name depth = TVar info name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onvar 0 t
                   where onvar c info name depth | name == j = termShift c s
                         onvar c info name depth = TVar info name depth

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
