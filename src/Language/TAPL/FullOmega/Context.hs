module Language.TAPL.FullOmega.Context where

import Data.Maybe (isJust)
import qualified Data.Map.Lazy as Map

import Text.Parsec (SourcePos)

import Control.Monad (when, unless)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Context
import Language.TAPL.FullOmega.Types

type LCNames = Names Binding
data LCState = LCState { names :: LCNames, memory :: LCMemory } deriving (Show)
type Eval a = StateT LCState (Except String) a

addName :: String -> LCNames -> LCNames
addName x = bind x NameBind

addVar :: String -> Type -> LCNames -> LCNames
addVar x ty = bind x (VarBind ty)

addTypeVar :: String -> Kind -> LCNames -> LCNames
addTypeVar x ty n = bind x (TypeVarBind ty) n

pickFreshName :: LCNames -> String -> (String, LCNames)
pickFreshName n x | isBound n x = pickFreshName n (x <> "'")
pickFreshName n x = (x, n') where n' = addName x n

getBinding :: SourcePos -> LCNames -> VarName -> Eval (Maybe Binding)
getBinding p names varName =
    case bindingType names varName of
         (Just binding) -> Just <$> bindingShift p (varName + 1) binding
         x -> return x

getTypeAbb :: SourcePos -> LCNames -> VarName -> Eval (Maybe Type)
getTypeAbb p names varName = do
    b <- getBinding p names varName
    case b of
         (Just (TypeAddBind ty _)) -> return $ Just ty
         _ -> return Nothing

isTypeAdd :: SourcePos -> LCNames -> VarName -> Eval Bool
isTypeAdd p names varName = isJust <$> getTypeAbb p names varName

bindingShift :: SourcePos -> VarName -> Binding -> Eval Binding
bindingShift _ _ NameBind = return NameBind
bindingShift _ _ (TypeVarBind k) = return $ TypeVarBind k
bindingShift p d (TypeAddBind ty k) = flip(TypeAddBind) k <$> typeShift p d ty
bindingShift p d (VarBind ty) = VarBind <$> typeShift p d ty

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

shiftStore :: SourcePos -> VarName -> Eval ()
shiftStore p i = do
    m <- getMemory
    m' <- sequence $ termShift p i <$> m
    putMemory m'

typeTermSubstitutionTop :: SourcePos -> Type -> Term -> Eval Term
typeTermSubstitutionTop p tyS t = do
    x <- typeShift p 1 tyS
    y <- typeTermSubstitution p x 0 t
    termShift p (-1) y

typeTermSubstitution p tyS j t =
    termMap (\p _ x n -> return $ TVar p x n)
            (\j tyT -> typeSubstitution p tyS j tyT) j t

termMap :: (SourcePos -> Int -> VarName -> Depth -> Eval Term) -> (Int -> Type -> Eval Type) -> Int -> Term -> Eval Term
termMap onVar onType s t = walk s t
                     where walk c (TAscribe p t ty) = TAscribe p <$> walk c t <*> onType c ty
                           walk c (TVar p name depth) = onVar p c name depth
                           walk c (TAbs p x ty t) = TAbs p x <$> onType c ty <*> walk (c+1) t
                           walk c (TApp p t1 t2) = TApp p <$> walk c t1 <*> walk c t2
                           walk c (TProj p r k) = flip(TProj p) k <$> walk c r

                           walk c (TRecord p fields) = do
                                fs <- traverse f (Map.toList fields)
                                return $ TRecord p $ Map.fromList fs
                                where f (k,v) = (,) k <$> walk c v

                           walk c (TString p s) = return $ TString p s
                           walk c (TUnit p) = return $ TUnit p
                           walk _ t@(TLoc _ _) = return t
                           walk c (TRef p t1) = TRef p <$> walk c t1
                           walk c (TDeref p t1) = TDeref p <$> walk c t1
                           walk c (TAssign p t1 t2) = TAssign p <$> walk c t1 <*> walk c t2
                           walk c (TFloat p t) = return $ TFloat p t
                           walk c (TTimesFloat p t1 t2) = TTimesFloat p <$> walk c t1 <*> walk c t2
                           walk c (TLet p x t1 t2) = TLet p x <$> walk c t1 <*> walk (c+1) t2
                           walk c (TTrue p) = return $ TTrue p
                           walk c (TFalse p) = return $ TFalse p
                           walk c (TIf p t1 t2 t3) = TIf p <$> walk c t1 <*> walk c t2 <*> walk c t3
                           walk c (TZero p) = return $ TZero p
                           walk c (TSucc p t) = TSucc p <$> walk c t
                           walk c (TPred p t) = TPred p <$> walk c t
                           walk c (TIsZero p t) = TIsZero p <$> walk c t
                           walk c (TFix p t) = TFix p <$> walk c t
                           walk c (TTAbs p x k t) = TTAbs p x k <$> walk (c+1) t
                           walk c (TTApp p t ty) = TTApp p <$> walk c t <*> onType c ty
                           walk c (TPack p ty1 t ty2) = TPack p <$> onType c ty1 <*> walk c t <*> onType c ty2
                           walk c (TUnpack p ty x t1 t2) = TUnpack p ty x <$> walk c t1 <*> walk (c + 2) t2

termShiftAbove :: SourcePos -> Depth -> VarName -> Term -> Eval Term
termShiftAbove p d c t = termMap onVar (typeShiftAbove p d) c t
                   where onVar p c x n | x >= c = return $ TVar p (x + d) (n + d)
                         onVar p c x n = return $ TVar p x (n + d)

termShift :: SourcePos -> VarName -> Term -> Eval Term
termShift p d t = termShiftAbove p d 0 t

termSubstitution :: VarName -> Term -> Term -> Eval Term
termSubstitution j s t = termMap onVar onType j t
                   where onVar p j x n | x == j = termShift p j s
                         onVar p j x n = return $ TVar p x n
                         onType j ty = return ty

termSubstitutionTop :: SourcePos -> Term -> Term -> Eval Term
termSubstitutionTop p s t = do
    x <- termShift p 1 s
    y <- termSubstitution 0 x t
    termShift p (-1) y

typeMap :: (Int -> VarName -> Depth -> Eval Type) -> Int -> Type -> Eval Type
typeMap onVar s g = walk s g
              where walk c (TyVar x n) = onVar c x n
                    walk _ (TyID x) = return $ TyID x
                    walk _ TyString = return TyString
                    walk _ TyUnit = return TyUnit
                    walk c (TyRef ty1) = TyRef <$> walk c ty1

                    walk c (TyRecord fields) = do
                        fs <- traverse f (Map.toList fields)
                        return $ TyRecord $ Map.fromList fs
                        where f (k,v) = (,) k <$> walk c v

                    walk _ TyFloat = return TyFloat
                    walk c (TyAll x k ty2) = TyAll x k <$> walk (c+1) ty2
                    walk _ TyBool = return TyBool
                    walk _ TyNat = return TyNat
                    walk c (TyArrow ty1 ty2) = TyArrow <$> walk c ty1 <*> walk c ty2
                    walk c (TySome x k ty1) = TySome x k <$> walk (c+1) ty1
                    walk c (TyAbs x k ty1) = TyAbs x k <$> walk (c+1) ty1
                    walk c (TyApp ty1 ty2) = TyApp <$> walk c ty1 <*> walk c ty2

typeSubstitution :: SourcePos -> Type -> VarName -> Type -> Eval Type
typeSubstitution p tyS j tyT = typeMap onVar j tyT
                    where onVar j x n | x == j = typeShift p j tyS
                          onVar j x n = return $ TyVar x n

typeSubstitutionTop :: SourcePos -> Type -> Type -> Eval Type
typeSubstitutionTop p tyS tyT = do
    x <- typeShift p 1 tyS
    y <- typeSubstitution p x 0 tyT
    typeShift p (-1) y

typeShift :: SourcePos -> VarName -> Type -> Eval Type
typeShift p d tyT = typeShiftAbove p d 0 tyT

typeShiftAbove :: SourcePos -> Depth -> VarName -> Type -> Eval Type
typeShiftAbove p d c ty = typeMap onVar c ty
                  where onVar c x n | x >= c = if x + d < 0
                                               then lift $ throwE $ show p <> " : attempt to use type variable in invalid scope"
                                               else return $ TyVar (x + d) (n + d)
                        onVar c x n = return $ TyVar x (n + d)
