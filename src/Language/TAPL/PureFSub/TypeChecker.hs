module Language.TAPL.PureFSub.TypeChecker (typeOf) where

import qualified Data.Map.Lazy as Map

import Text.Parsec (SourcePos)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Context (bind)
import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.PureFSub.Types
import Language.TAPL.PureFSub.Pretty
import Language.TAPL.PureFSub.Context

data TypeError = TypeMissmatch SourcePos String

typeOf :: Term -> Eval Type
typeOf (TTAbs p tyX tyT1 t2) = withTmpStateT (bind tyX (TypeAddBind tyT1)) $ do { TyAll tyX tyT1 <$> typeOf t2 }

typeOf (TTApp p t1 tyT2) = do
    tyT1 <- lcst =<< typeOf t1
    case tyT1 of
        TyAll _ tyT11 tyT12 -> do
            unlessM (tyT2 <: tyT11) (typeError p "type parameter type mismatch")
            return $ typeSubstitutionTop tyT2 tyT12
        _ -> typeError p "universal type expected"

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

typeOf r@(TApp p t1 t2) = do
    tyT1 <- lcst =<< typeOf t1
    tyT2 <- typeOf t2
    s <- get
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            unlessM (tyT2 <: tyT11) (typeError p $ "parameter type mismatch")
            return tyT12
         x -> typeError p "arrow type expected"

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

unexpectedType :: SourcePos -> Type -> Type -> Eval a
unexpectedType p expected actual = do
    tyE <- prettifyType expected
    tyA <- prettifyType actual
    typeError p $ "expected type " <> show tyE <> ", actual " <> show tyA

instance Show TypeError where
    show (TypeMissmatch p message) = show p <> ":" <> message

(<:) :: Type -> Type -> Eval Bool
(<:) tyS tyT | tyS == tyT = return True
(<:) tyS tyT = case (tyS, tyT) of
                    (TyVar _ _, _) -> do
                        x <- promote tyS
                        case x of
                             Just ty -> ty <: tyT
                             Nothing -> return False

                    (TyAll tyX tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                        x <- (&&) <$> (tyS2 <: tyT1) <*> (tyT1 <: tyS1)
                        if x
                        then withTmpStateT (addVar tyX tyT1) $ do { tyS2 <: tyT2 }
                        else return False

                    (_, TyTop) -> return True
                    (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyT1 <: tyS1) <*> (tyS2 <: tyT2)
                    (_, _) -> return False

promote :: Type -> Eval (Maybe Type)
promote (TyVar i _) = do
    n <- get
    case getBinding n i of
         Just (TypeAddBind ty) -> return $ Just ty
         _ -> return Nothing

promote _ = return Nothing

lcst :: Type -> Eval Type
lcst ty = do
    x <- promote ty
    case x of
        Just ty -> lcst ty
        Nothing -> return ty
