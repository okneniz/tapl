module Language.TAPL.Bot.TypeChecker where

import qualified Data.Map.Strict as Map
import Data.List (tails, (\\), intercalate)

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context

typeOf :: Term -> Eval Type
typeOf (TVar info v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError info "var type error"

typeOf (TAbs _ name ty t) = do
    withTmpStateT (addVar name ty) $ do
        ty' <- typeOf t
        return $ TyArrow ty ty'

typeOf (TApp info t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    case ty1 of
         (TyArrow ty1' ty2') | ty2 <: ty1' -> return ty2'
         (TyArrow ty1' _) -> typeError info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
         TyBot -> return TyBot
         _ -> typeError info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

typeError :: Info -> String -> Eval a
typeError info message = lift $ throwE $ show info ++ ":" ++ message

