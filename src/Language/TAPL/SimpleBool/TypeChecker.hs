module Language.TAPL.SimpleBool.TypeChecker where

import Control.Monad (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Text.Parsec (SourcePos)

import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Context

type Inferred a = ExceptT TypeError (State LCNames) a
data TypeError = TypeMissmatch SourcePos String

typeOf :: LCNames -> Term -> Either String Type
typeOf names term =
    case evalState (runExceptT (infer term)) names of
         Left x -> Left $ show x
         Right x -> return x

infer :: Term -> Inferred Type
infer (TTrue _) = return TyBool
infer (TFalse _) = return TyBool

infer (TIf pos t1 t2 t3) = do
  ty1 <- infer t1
  case ty1 of
       TyBool -> do
          ty2 <- infer t2
          ty3 <- infer t3
          if ty2 == ty3
          then return ty2
          else throwE $ TypeMissmatch pos $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")"
       ty -> throwE $ TypeMissmatch pos $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty ++ ")"

infer (TVar info v _) = do
  names <- lift $ get
  case liftM snd $ pickVar names v of
       Just (VarBind ty') -> return ty'
       Just x -> throwE $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show names ++ " " ++ show v ++ ")"
       Nothing -> throwE $ TypeMissmatch info $ "var type error"

infer (TApp info t1 t2) = do
  ty1 <- infer t1
  ty2 <- infer t2
  case ty1 of
       (TyArrow ty1' ty2') ->
          if ty2 == ty1'
          then return ty2'
          else throwE $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
       _ -> throwE $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

infer (TAbs _ name ty t) = do
  names <- lift $ get
  lift $ modify $ bind name (VarBind ty)
  ty' <- infer t
  lift $ put names
  return $ TyArrow ty ty'

instance Show TypeError where
    show (TypeMissmatch pos message) = message ++ " in " ++ show pos
