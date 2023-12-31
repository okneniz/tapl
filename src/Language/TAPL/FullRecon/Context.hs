module Language.TAPL.FullRecon.Context where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.FullRecon.Types
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

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName c name | isBound c name = pickFreshName c (name <> "'")
pickFreshName c name = (name, c') where c' = addName name c

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
              where walk c (TVar p name depth) = onVar c p name depth
                    walk c (TAbs p x ty t1) = TAbs p x ty (walk (c+1) t1)
                    walk c (TApp p t1 t2) = TApp p (walk c t1) (walk c t2)
                    walk c (TIf p t1 t2 t3) = TIf p (walk c t1) (walk c t2) (walk c t3)
                    walk _ (TTrue p) = TTrue p
                    walk _ (TFalse p) = TFalse p
                    walk _ (TZero p) = TZero p
                    walk c (TIsZero p t1) = TIsZero p (walk c t1)
                    walk c (TPred p t1) = TPred p (walk c t1)
                    walk c (TSucc p t1) = TSucc p (walk c t1)
                    walk c (TLet p x t1 t2) = TLet p x (walk c t1) (walk (c+1) t2)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = termMap onVar s t
                 where onVar c p name depth | name >= c = TVar p (name + d) (depth + d)
                       onVar _ p name depth = TVar p name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onvar 0 t
                   where onvar c _ name _ | name == j = termShift c s
                         onvar _ p name depth = TVar p name depth

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
