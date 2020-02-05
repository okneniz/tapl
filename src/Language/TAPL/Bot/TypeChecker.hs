module Language.TAPL.Bot.TypeChecker where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context

type Inferred a = ExceptT TypeError (State LCNames) a

typeOf :: LCNames -> Term -> Either String Type
typeOf names term =
    case evalState (runExceptT (infer term)) names of
         Left x -> Left $ show x
         Right x -> return x

infer :: Term -> Inferred Type
infer v@(TVar info varname _) = do
  names <- lift $ get
  case liftM snd $ pickVar names varname of
       Just (VarBind ty') -> return ty'
       Just x -> throwE $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show names ++ " " ++ show v ++ ")"
       Nothing -> throwE $ TypeMissmatch info $ "var type error"

infer (TAbs _ name ty t) = do
    names <- lift $ get
    lift $ modify $ bind name (VarBind ty)
    ty' <- infer t
    lift $ put names
    return $ TyArrow ty ty'

infer (TApp info t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    case ty1 of
         (TyArrow ty1' ty2') | ty2 <: ty1' -> return ty2'
         (TyArrow ty1' _) -> throwE $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
         TyBot -> return TyBot
         _ -> throwE $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

data TypeError = TypeMissmatch Info String

instance Show TypeError where
    show (TypeMissmatch info message) = message ++ " in " ++ (show $ row info) ++ ":" ++ (show $ column info)


(<:) :: Type -> Type -> Bool
_ <: TyTop = True
TyBot <: _ = True
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
x <: y | x == y = True
_ <: _ = False