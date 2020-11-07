module Language.TAPL.Recon.TypeReconstructor (reconstruct) where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Context (pickVar)
import Language.TAPL.Common.Helpers
import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Context

reconstruct :: Term -> Eval Type
reconstruct = recover

recover :: Term -> Eval Type
recover (TTrue _) = return TyBool
recover (TFalse _) = return TyBool
recover (TZero _) = return TyNat

recover (TSucc _ t1) = do
    tyT1 <- recover t1
    prependConstraint (tyT1,TyNat)
    return TyNat

recover (TPred _ t1) = do
    tyT1 <- recover t1
    prependConstraint (tyT1,TyNat)
    return TyNat

recover (TIsZero _ t1) = do
    tyT1 <- recover t1
    prependConstraint (tyT1,TyNat)
    return TyBool

recover (TIf _ t1 t2 t3) = do
    tyT1 <- recover t1
    tyT2 <- recover t2
    tyT3 <- recover t3
    prependConstraint (tyT2,tyT3)
    prependConstraint (tyT1,TyBool)
    return tyT3

recover (TVar p varName _) = do
    n <- getNames
    case pickVar n varName of
         Just (_, VarBind ty) -> return ty
         _ -> lift $ throwE $ show $ TypeMissmatch p "Wrong type of binding"

recover (TAbs _ x tyT1 t2) =
    withTmpStateT (putVar x tyT1) $ do
        tyT2 <- recover t2
        return $ TyArrow tyT1 tyT2

recover (TApp _ t1 t2) = do
    tyT1 <- recover t1
    tyT2 <- recover t2
    tyX <- newVar
    appendConstraint (tyT1, TyArrow tyT2 (TyID tyX))
    return $ TyID tyX

newVar :: Eval String
newVar = do
    s <- get
    let x = "x" <> show (varIndex s)
    put $ s { varIndex = ((varIndex s) + 1) }
    return x

prependConstraint :: (Type, Type) -> Eval ()
prependConstraint c = modify $ \s -> s { constraints = [c] <> (constraints s) }

appendConstraint :: (Type, Type) -> Eval ()
appendConstraint c = modify $ \s -> s { constraints = (constraints s) <> [c] }
