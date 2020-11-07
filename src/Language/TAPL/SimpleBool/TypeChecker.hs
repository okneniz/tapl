module Language.TAPL.SimpleBool.TypeChecker where

import Text.Parsec (SourcePos)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Context
import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Pretty
import Language.TAPL.SimpleBool.Context

typeOf :: Term -> Eval Type
typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf pos t1 t2 t3) = do
  ty1 <- typeOf t1
  case ty1 of
       TyBool -> do
          ty2 <- typeOf t2
          ty3 <- typeOf t3
          if ty2 == ty3
          then return ty2
          else typeError pos $ "branches of condition have different types (" <> show ty2 <> " and " <> show ty3 <> ")"
       ty -> typeError pos $ "guard of condition have not a " <> show TyBool <>  " type (" <> show ty <> ")"

typeOf (TVar pos v _) = do
  names <-get
  case bindingType names v of
       Just (VarBind ty') -> return ty'
       Just x -> typeError pos $ "wrong kind of binding for variable (" <> show x <> " " <> show names <> " " <> show v <> ")"
       Nothing -> typeError pos $ "var type error"

typeOf (TApp pos t1 t2) = do
  ty1 <- typeOf t1
  ty2 <- typeOf t2
  case ty1 of
       (TyArrow ty1' ty2') ->
          if ty2 == ty1'
          then return ty2'
          else typeError pos $ "incorrect application of abstraction " <> show ty2 <> " to " <> show ty1'
       _ -> typeError pos $ "incorrect application " <> show ty1 <> " and " <> show ty2

typeOf (TAbs _ name ty t) = do
  names <-get
  modify $ addVar name ty
  ty' <- typeOf t
  put names
  return $ TyArrow ty ty'

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

data TypeError = TypeMissmatch SourcePos String

instance Show TypeError where
    show (TypeMissmatch pos message) = message <> " in " <> show pos
