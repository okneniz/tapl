module Language.TAPL.FOmega.TypeChecker (typeOf, kindOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate, sort)
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.FOmega.Pretty (render, renderType)

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Context
import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FOmega.Types
import Language.TAPL.FOmega.Context

typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty))-> return ty
         (Just x)-> typeError p $ "wrong kind of binding for variable " ++ show x
         Nothing -> typeError p "var type error"

typeOf (TAbs p x tyT1 t2) = do
    unlessM (isStar p tyT1) (typeError p "Kind * expected")
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 $ typeShift (-1) tyT2

typeOf r@(TApp p t1 t2) = do
    tyT1 <- simplifyType =<< typeOf t1
    tyT2 <- typeOf t2
    n <- get
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            tyT22 <- simplifyType tyT2
            unlessM (typeEq tyT2 tyT11) (typeError p $ "parameter type missmatch " ++ show tyT2 ++ "  " ++ show tyT11)
            return tyT12
         x -> typeError p $ "arrow type expected " ++ show x ++ " : " ++ show n

typeOf (TTAbs p x k t) = withTmpStateT (addTypeVar x k) $ TyAll x k <$> typeOf t

typeOf (TTApp p t1 tyT2) = do
    knKT2 <- kindOf p tyT2
    tyT1 <- simplifyType =<< typeOf t1
    case tyT1 of
        TyAll _ knK11 tyT12 | knK11 /= knKT2 -> typeError p "Type argument has wrong kind"
        TyAll _ knK11 tyT12 -> return $ typeSubstitutionTop tyT2 tyT12
        _ -> typeError p "universal type expected"

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
    where message = "Argument error, expected " ++ show expected  ++ ". Got " ++ show actual ++ "."

isStar :: SourcePos -> Type -> Eval Bool
isStar p ty = (==) Star <$> kindOf p ty

getKind :: SourcePos -> VarName -> Eval Kind
getKind p v = do
    n <- get
    case getBinding n v of
        (Just (TypeVarBind k)) -> return k
        _ -> lift $ throwE $ "Wrong kind of binding for variable  " ++ show v ++ " : " ++ show n

kindOf :: SourcePos -> Type -> Eval Kind
kindOf p (TyVar i _) = getKind p i

kindOf p (TyAll tyX knK1 tyT2) = do
    withTmpStateT (addTypeVar tyX knK1) $ do
        unlessM (isStar p tyT2) (typeError p "star kind expected")
        return Star

kindOf p (TyAbs tyX knK1 tyT2) = withTmpStateT (addTypeVar tyX knK1) $ Arrow knK1 <$> kindOf p tyT2

kindOf p x@(TyApp tyT1 tyT2) = do
    knK1 <- kindOf p tyT1
    knK2 <- kindOf p tyT2
    names <- get
    case knK1 of
         (Arrow knK11 knK12) | knK2 == knK11 -> return knK12
         (Arrow knK11 knK12) -> typeError p "parameter kind mismatch"
         _ -> typeError p $ "arrow kind expected " ++ show x ++ " - " ++ show knK1 ++ show knK2 ++ " : " ++ show names

kindOf p (TyArrow tyT1 tyT2) = do
    unlessM (isStar p tyT1) (typeError p "star kind expected")
    unlessM (isStar p tyT2) (typeError p "star kind expected")
    return Star

kindOf _ _ = return Star

typeEq :: Type -> Type -> Eval Bool
typeEq t1 t2 = do
    tyS <- simplifyType t1
    tyT <- simplifyType t2
    n <- get
    case (tyS, tyT) of
        (TyVar i _, TyVar j _) -> return $ i == j

        (TyAll x k1 ty1, TyAll _ k2 ty2) | k1 == k2 ->
            withTmpStateT (addName x) $ typeEq ty1 ty2
        (TyAll x k1 ty1, TyAll _ k2 ty2) -> return False

        (TyAbs x k1 ty1, TyAbs _ k2 ty2) | k1 == k2 ->
            withTmpStateT (addName x) $ typeEq ty1 ty2
        (TyAbs x k1 ty1, TyAbs _ k2 ty2) -> return False

        (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
        (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
        _ -> return False

computeType :: Type -> Eval (Maybe Type)
computeType (TyApp (TyAbs _ _ ty1) ty2) = return.return $ typeSubstitutionTop ty2 ty1
computeType _ = return Nothing

simplifyType :: Type -> Eval Type
simplifyType z@(TyApp ty1 ty2) = do
    tyX <- flip(TyApp) ty2 <$> simplifyType ty1
    n <- computeType tyX
    case n of
         Just x -> simplifyType x
         _ -> return tyX

simplifyType ty = do
    n <- computeType ty
    case n of
         Just x -> simplifyType x
         _ -> return ty
