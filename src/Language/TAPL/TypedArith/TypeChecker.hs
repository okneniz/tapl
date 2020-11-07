module Language.TAPL.TypedArith.TypeChecker where

import Data.List (tails, intercalate, all, (\\), sort)
import Text.Parsec (SourcePos)

import qualified Data.Map.Lazy as Map

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.Common.Context
import Language.TAPL.TypedArith.Types
import Language.TAPL.TypedArith.Pretty
import Language.TAPL.TypedArith.Context

typeOf :: Term -> Eval Type
typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool
typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
  ty <- typeOf t
  case ty of
      TyNat -> return TyNat
      x -> typeError p $ "argument of succ is not a natural number (" <> show x <> ")"

typeOf (TPred p t) = do
  ty <- typeOf t
  case ty of
     TyNat -> return TyNat
     x -> typeError p $ "argument of pred is not a natural number (" <> show x <> ")"

typeOf (TIsZero p t) = do
  ty <- typeOf t
  case ty of
    TyNat -> return TyBool
    x -> typeError p $ "argument of zero? is not a natural number (" <> show x <> ")"

typeOf (TIf p t1 t2 t3) = do
  ty1 <- typeOf t1
  ty2 <- typeOf t2
  ty3 <- typeOf t3
  case ty1 of
       TyBool ->
            if ty2 == ty3
            then return ty2
            else typeError p $ "branches of condition have different types (" <> show ty2 <> " and " <> show ty3 <> ")"
       ty -> typeError p $ "guard of condition have not a " <> show TyBool <>  " type (" <> show ty <> ")"

typeOf v@(TVar p varname _) = do
  names <- get
  case bindingType names varname of
       Just (VarBind ty') -> return ty'
       Just x -> typeError p $ "wrong kind of binding for variable (" <> show x <> " " <> show names <> " " <> show v <> ")"
       Nothing -> typeError p $ "var type error"

typeOf (TApp p t1 t2) = do
  ty1 <- typeOf t1
  ty2 <- typeOf t2
  case ty1 of
       (TyArrow ty1' ty2') ->
            if ty2 == ty1'
            then return ty2'
            else typeError p $ "incorrect application of abstraction " <> show ty2 <> " to " <> show ty1'
       _ -> typeError p $ "incorrect application " <> show ty1 <> " and " <> show ty2

typeOf (TAbs _ name ty t) = do
  names <- get
  modify $ addVar name ty
  ty' <- typeOf t
  put names
  return $ TyArrow ty ty'

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

unexpectedType :: SourcePos -> Type -> Type -> Eval a
unexpectedType p expected actual = do
    tyE <- prettifyType expected
    tyA <- prettifyType actual
    typeError p $ "expected type " <> show tyE <> ", actual " <> show tyA

data TypeError = TypeMissmatch SourcePos String

instance Show TypeError where
    show (TypeMissmatch pos message) = message <> " in " <> show pos
