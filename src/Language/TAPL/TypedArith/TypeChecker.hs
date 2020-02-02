{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.TAPL.TypedArith.TypeChecker where

import Language.TAPL.TypedArith.Types
import Language.TAPL.TypedArith.Context
import Control.Monad (liftM)

import Prelude hiding (abs, succ, pred)
import Data.List (intercalate, all, nub, (\\), find, sortBy)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

class LCContext c => TypeChecker c where
  typeFromContext :: c -> VarName -> Maybe Binding
  isCorrect :: c -> Bool
  typeOf :: c -> Either TypeError Type

instance TypeChecker (TypedArithContext Term) where
  typeFromContext context name = liftM snd $ pickVar context name
  isCorrect c = isRight $ typeOf c

  typeOf (TypedArithContext _ (TTrue _)) = return TyBool
  typeOf (TypedArithContext _ (TFalse _)) = return TyBool
  typeOf (TypedArithContext _ (TZero _)) = return TyNat

  typeOf (TypedArithContext n (TSucc info t)) = do
      ty <- typeOf $ TypedArithContext n t
      case ty of
          TyNat -> return TyNat
          ty -> Left $ TypeMissmatch info $ "argument of succ is not a natural number (" ++ show ty ++ ")"

  typeOf (TypedArithContext n (TPred info t)) = do
      ty <- typeOf $ TypedArithContext n t
      case ty of
         TyNat -> return TyNat
         ty -> Left $ TypeMissmatch info $ "argument of pred is not a natural number (" ++ show ty ++ ")"

  typeOf (TypedArithContext n (TIsZero info t)) = do
      ty <- typeOf $ TypedArithContext n t
      case ty of
        TyNat -> return TyBool
        ty -> Left $ TypeMissmatch info $ "argument of zero? is not a natural number (" ++ show ty ++ ")"

  typeOf (TypedArithContext n (TIf info t1 t2 t3)) = do
      ty1 <- typeOf $ TypedArithContext n t1
      ty2 <- typeOf $ TypedArithContext n t2
      ty3 <- typeOf $ TypedArithContext n t3
      case ty1 of
           TyBool -> if ty2 == ty3
                     then return ty2
                     else Left $ TypeMissmatch info $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")"
           ty -> Left $ TypeMissmatch info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty ++ ")"

  typeOf c@(TypedArithContext n v@(TVar info varname depth)) =
      let ty = typeFromContext c varname
      in case ty of
              Just (VarBind ty') -> return ty'
              Just x -> Left $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
              Nothing -> Left $ TypeMissmatch info $ "var type error"

  typeOf (TypedArithContext n (TApp info t1 t2)) = do
      ty1 <- typeOf $ TypedArithContext n $ t1
      ty2 <- typeOf $ TypedArithContext n $ t2
      case ty1 of
           (TyArrow ty1' ty2') -> if ty2 == ty1'
                                  then return ty2'
                                  else Left $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
           x -> Left $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

  typeOf c@(TypedArithContext n (TAbs _ name ty t)) = do
      ty' <- typeOf $ bind (TypedArithContext n t) name (VarBind ty)
      return $ TyArrow ty ty'
