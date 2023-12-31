module Language.TAPL.Equirec.TypeChecker (typeOf) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Helpers (withTmpStateT, ok, nvm)
import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Context

typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" <> show x <> " " <> show n <> " " <> show v <> ")"
         Nothing -> typeError p "var type error"

typeOf (TAbs _ x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

typeOf (TApp p t1 t2) = do
    tyT1 <- typeOf t1
    tyT2 <- typeOf t2
    tyT1' <- simplifyType tyT1
    case tyT1' of
         (TyArrow tyT11 tyT12) -> do
            x <- typeEq tyT2 tyT11
            if x
            then return tyT12
            else typeError p $ "incorrect application of abstraction " <> show tyT2 <> " to " <> show tyT11
         _ -> typeError p $ "incorrect application " <> show tyT1 <> " and " <> show tyT2

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

typeEq :: Type -> Type -> Eval Bool
typeEq tyS tyT = do
    names <- get
    return $ typeEq' [] names tyS tyT
    where mem _ [] = False
          mem y (x:xs) = (y == x) || (mem y xs)
          typeEq' seen ns ty1 ty2 = if mem (tyS,tyT) seen then True else typeEq'' [] ns ty1 ty2
          typeEq'' seen n (TyRec _ tyS1) _ = typeEq' ((tyS, tyT):seen) n (typeSubstitutionTop tyS tyS1) tyT
          typeEq'' seen n _ (TyRec _ tyT1) = typeEq' ((tyS, tyT):seen) n tyS (typeSubstitutionTop tyT tyT1)
          typeEq'' _ _ (TyID b1) (TyID b2) = b1 == b2
          typeEq'' seen n (TyArrow tyS1 tyS2) (TyArrow tyT1 tyT2) = (typeEq' seen n tyS1 tyT1) && (typeEq' seen n tyS2 tyT2)
          typeEq'' _ _ _ _ = False

computeType :: Type -> Eval (Maybe Type)
computeType ty@(TyRec _ tyS) = do
    return $ Just $ typeSubstitutionTop ty tyS

computeType (TyVar i _) = do
    n <- get
    if isTypeAbb n i
    then return $ getTypeAbb n i
    else nvm

computeType _ = nvm

simplifyType :: Type -> Eval Type
simplifyType ty = do
    n <- computeType ty
    case n of
         Just x -> simplifyType x
         _ -> return ty
