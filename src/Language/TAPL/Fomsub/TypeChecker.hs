module Language.TAPL.Fomsub.TypeChecker (typeOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate, sort)
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Control.Monad (when, unless, liftM, liftM2, when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.Common.Context
import Language.TAPL.Fomsub.Types
import Language.TAPL.Fomsub.Context


typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty))-> return ty
         (Just x)-> typeError p $ "wrong kind of binding for variable " <> show x
         Nothing -> typeError p "var type error"

typeOf (TAbs p x tyT1 t2) = do
    unlessM (isStar p tyT1) (typeError p "Kind * expected")
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 $ typeShift (-1) tyT2

typeOf r@(TApp p t1 t2) = do
    tyT1 <- lcst =<< typeOf t1
    tyT2 <- typeOf t2
    n <- get
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            unlessM (tyT2 <: tyT11) (typeError p $ "parameter type missmatch " <> show tyT2 <> "  " <> show tyT11)
            return tyT12
         x -> typeError p $ "arrow type expected " <> show x <> " : " <> show n

typeOf (TTAbs p x ty t) = withTmpStateT (addTypeVar x ty) $ TyAll x ty <$> typeOf t

typeOf (TTApp p t1 tyT2) = do
    tyT1 <- lcst =<< typeOf t1
    case tyT1 of
         TyAll _ tyT11 tyT12 -> do
             unlessM (tyT2 <: tyT11) (typeError p $ "type parameter type mismatch")
             return $ typeSubstitutionTop tyT2 tyT12
         _ -> typeError p "universal type expected"

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
    where message = "Argument error, expected " <> show expected  <> ". Got " <> show actual <> "."

isStar :: SourcePos -> Type -> Eval Bool
isStar p ty = (==) Star <$> kindOf p ty

getKind :: SourcePos -> VarName -> Eval Kind
getKind p v = do
    n <- get
    case getBinding n v of
        (Just (TypeVarBind ty)) -> kindOf p ty
        _ -> lift $ throwE $ show p <> " : wrong kind of binding for variable  " <> show v <> " : " <> show n

kindOf :: SourcePos -> Type -> Eval Kind
kindOf p (TyVar i _) = getKind p i

kindOf p (TyAll tyX tyT1 tyT2) = do
    withTmpStateT (addTypeVar tyX tyT1) $ do
        unlessM (isStar p tyT2) (typeError p "kind * expected")
        return Star

kindOf p (TyAbs tyX knK1 tyT2) = withTmpStateT (addTypeVar tyX (makeTop knK1)) $ Arrow knK1 <$> kindOf p tyT2

kindOf p x@(TyApp tyT1 tyT2) = do
    knK1 <- kindOf p tyT1
    knK2 <- kindOf p tyT2
    names <- get
    case knK1 of
         (Arrow knK11 knK12) | knK2 == knK11 -> return knK12
         (Arrow knK11 knK12) -> typeError p "parameter kind mismatch"
         _ -> typeError p $ "arrow kind expected " <> show x <> " - " <> show knK1 <> show knK2 <> " : " <> show names

kindOf p (TyArrow tyT1 tyT2) = do
    unlessM (isStar p tyT1) (typeError p "star kind expected")
    unlessM (isStar p tyT2) (typeError p "star kind expected")
    return Star

kindOf _ _ = return Star

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    tyS <- simplifyType ty1
    tyT <- simplifyType ty2
    n <- get
    case (tyS, tyT) of
      (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
      (TyTop, TyTop) -> return True
      (TyVar i _, TyVar j _) | i == j -> return True
      (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> withTmpStateT (addName tyX1) $ do
        (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
      (TyAbs tyX1 k1 tyS2, TyAbs _ k2 tyT2) | k1 == k2 -> withTmpStateT (addName tyX1) $ typeEq tyS2 tyT2
      (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
      _ -> return False

(<:) :: Type -> Type -> Eval Bool
(<:) tyS tyT = do
    x <- typeEq tyS tyT
    if x
    then return True
    else do
        tyS' <- simplifyType tyS
        tyT' <- simplifyType tyT
        case (tyS', tyT') of
              (_, TyTop) -> return True
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyS1 <: tyT1) <*> (tyS2 <: tyT2)

              (TyVar _ _, _) -> do
                x <- promote tyS'
                case x of
                     Just ty -> ty <: tyT
                     Nothing -> return False

              (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                    x <- (&&) <$> (tyS1 <: tyT1) <*> (tyT1 <: tyS1)
                    if x
                    then withTmpStateT (addTypeVar tyX1 tyT1) $ tyS2 <: tyT2
                    else return False

              (TyAbs x k1 tyS2, TyAbs _ k2 tyT2) | k1 == k2 ->
                 withTmpStateT (addTypeVar x (makeTop k1)) $ tyS2 <: tyT2

              (TyApp _ _, _) -> do
                    x <- promote tyS'
                    case x of
                         Just ty -> ty <: tyT
                         Nothing -> return False

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

promote :: Type -> Eval (Maybe Type)
promote (TyVar i _) = do
    n <- get
    case getBinding n i of
         Just (TypeVarBind ty) -> return $ Just ty
         _ -> return Nothing

promote (TyApp tyS tyT) = fmap(flip(TyApp) tyT) <$> promote tyS
promote _ = return Nothing

lcst :: Type -> Eval Type
lcst ty = do
    tyS <- simplifyType ty
    x <- promote tyS
    case x of
        Just ty -> lcst ty
        Nothing -> return tyS
