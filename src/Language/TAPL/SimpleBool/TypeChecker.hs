{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.TAPL.SimpleBool.TypeChecker where

import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Context
import Data.Either (isRight)
import Control.Monad (liftM)

class LCContext c => TypeChecker c where
  typeFromContext :: c -> VarName -> Maybe Binding
  isCorrect :: c -> Bool
  typeOf :: c -> Either TypeError Type

instance TypeChecker (SimpleBoolContext Term) where
  typeFromContext context name = liftM snd $ pickVar context name
  isCorrect c = isRight $ typeOf c

  typeOf (SimpleBoolContext _ (TTrue _)) = return TyBool
  typeOf (SimpleBoolContext _ (TFalse _)) = return TyBool

  typeOf (SimpleBoolContext n (TIf info t1 t2 t3)) = do
      ty1 <- typeOf $ SimpleBoolContext n t1
      ty2 <- typeOf $ SimpleBoolContext n t2
      ty3 <- typeOf $ SimpleBoolContext n t3
      case ty1 of
           TyBool -> if ty2 == ty3
                     then return ty2
                     else Left $ TypeMissmatch info $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")"
           ty -> Left $ TypeMissmatch info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty ++ ")"

  typeOf c@(SimpleBoolContext n v@(TVar info varname depth)) =
      let ty = typeFromContext c varname
      in case ty of
              Just (VarBind ty') -> return ty'
              Just x -> Left $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
              Nothing -> Left $ TypeMissmatch info $ "var type error"

  typeOf (SimpleBoolContext n (TApp info t1 t2)) = do
      ty1 <- typeOf $ SimpleBoolContext n $ t1
      ty2 <- typeOf $ SimpleBoolContext n $ t2
      case ty1 of
           (TyArrow ty1' ty2') -> if ty2 == ty1'
                                  then return ty2'
                                  else Left $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
           x -> Left $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

  typeOf c@(SimpleBoolContext n (TAbs _ name ty t)) = do
      let t' = bind (SimpleBoolContext n t) name (VarBind ty)
      ty' <- typeOf t'
      return $ TyArrow ty ty'
