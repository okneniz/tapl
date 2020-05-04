module Language.TAPL.FullError.TypeChecker (typeOf) where

import Data.List (tails, (\\), intercalate)

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.FullError.Types
import Language.TAPL.FullError.Context

typeOf :: Term -> Eval Type
typeOf = infer

infer :: Term -> Eval Type
infer (TTrue _) = return TyBool
infer (TFalse _) = return TyBool

infer (TVar info v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError info "var type error"

infer (TAbs _ x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- infer t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

infer (TApp info t1 t2) = do
    tyT1 <- infer t1
    tyT2 <- infer t2
    tyT1' <- simplifyType tyT1
    case tyT1' of
         (TyArrow tyT11 tyT12) -> do
            unlessM (tyT2 <: tyT11)
                    (typeError info $ "incorrect application of abstraction " ++ show tyT2 ++ " to " ++ show tyT11)
            return tyT12
         TyBot -> return TyBot
         _ -> typeError info $ "arrow type expected"

infer (TIf info t1 t2 t3) = do
    ty1 <- infer t1
    unlessM (ty1 <: TyBool)
            (typeError info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty1 ++ ")")
    ty2 <- infer t2
    ty3 <- infer t3
    joinTypes ty2 ty3

infer (TError _) = return TyBot

infer (TTry _ t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    joinTypes ty1 ty2

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = do
    x <- p
    unless x s

typeError :: Info -> String -> Eval a
typeError info message = lift $ throwE $ show info ++ ":" ++ message

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    ty1' <- simplifyType ty1
    ty2' <- simplifyType ty2
    n <- get
    case (ty1', ty2') of
        (TyTop, TyTop) -> return True
        (TyBot, TyBot) -> return True
        (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1
                                                       <*> typeEq tyS2 tyT2
        (TyBool, TyBool) -> return True

        (TyVar _ i, _) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x ty2'
                _ -> return False

        (_, TyVar _ i) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x ty1'
                _ -> return False

        (TyVar _ i, TyVar _ j) | i == j -> return True
        _ -> return False

(<:) :: Type -> Type -> Eval Bool
(<:) tyS tyT = do
    tyS' <- simplifyType tyS
    tyT' <- simplifyType tyT
    x <- typeEq tyS' tyT'
    if x
    then return True
    else case (tyS', tyT) of
              (_, TyTop) -> return True
              (TyBot, _) -> return False
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyS1 <: tyT1)
                                                             <*> (tyS2 <: tyT2)
              _ -> return False

joinTypes :: Type -> Type -> Eval Type
joinTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyT
    else do y <- tyT <: tyS
            if y
            then return tyS
            else do tyS' <- simplifyType tyS
                    tyT' <- simplifyType tyT
                    case (tyS',tyT') of
                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            ty1 <- meetTypes tyS1 tyT1
                            ty2 <- joinTypes tyS2 tyT2
                            return $ TyArrow ty1 ty2
                         _ -> return TyTop

meetTypes :: Type -> Type -> Eval Type
meetTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyT
    else do y <- tyT <: tyS
            if y
            then return tyS
            else do tyS' <- simplifyType tyS
                    tyT' <- simplifyType tyT
                    case (tyS',tyT') of
                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            ty1 <- joinTypes tyS1 tyT1
                            ty2 <- meetTypes tyS2 tyT2
                            return $ TyArrow ty1 ty2
                         _ -> return TyBot

computeType :: Type -> Eval (Maybe Type)
computeType (TyVar i _) = do
    n <- get
    if isTypeAbb n i
    then return $ getTypeAbb n i
    else return Nothing

computeType _ = return Nothing

simplifyType :: Type -> Eval Type
simplifyType ty = do
    n <- computeType ty
    case n of
         Just x -> simplifyType x
         _ -> return ty
