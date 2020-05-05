module Language.TAPL.RcdSubBot.TypeChecker (typeOf) where

import qualified Data.Map.Strict as Map

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.RcdSubBot.Types
import Language.TAPL.RcdSubBot.Context

typeOf :: Term -> Eval Type
typeOf = infer

infer :: Term -> Eval Type
infer (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = do
            tyf <- infer v
            return (k, tyf)

infer (TVar info v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError info "var type error"

infer (TAbs _ x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- infer t2
        return $ TyArrow tyT1 tyT2

infer (TApp info t1 t2) = do
    tyT1 <- infer t1
    tyT2 <- infer t2
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            unless (tyT2 <: tyT11)
                   (typeError info $ "incorrect application of abstraction " ++ show tyT2 ++ " to " ++ show tyT11)
            return tyT12
         TyBot -> return TyBot
         _ -> typeError info $ "arrow type expected"

infer (TProj info t1 (TKeyword _ key)) = do
    ty1 <- infer t1
    case ty1 of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError info $ "label " ++ show key ++ " not found"
         TyBot -> return TyBot
         _ -> typeError info $ "expected record type"

typeError :: Info -> String -> Eval a
typeError info message = lift $ throwE $ show info ++ ":" ++ message

(<:) :: Type -> Type -> Bool
(<:) tyS tyT | tyS == tyT = True
(<:) _ TyTop = True
(<:) TyBot _ = True
(<:) (TyArrow tyS1 tyS2) (TyArrow tyT1 tyT2) = (tyT1 <: tyS1) && (tyS2 <: tyT2)
(<:) (TyRecord fields1) (TyRecord fields2) = all (\(ty1,ty2) -> ty1 <: ty2)
                                           $ Map.elems
                                           $ Map.intersectionWith (,) fields1 fields2
(<:) _ _ = False