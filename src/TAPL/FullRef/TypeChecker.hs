{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullRef.TypeChecker where
import TAPL.FullRef.Types
import TAPL.FullRef.Context

import Control.Monad (liftM)
import Data.Either (isRight, isLeft)

import Prelude hiding (lookup)
import qualified Prelude (lookup)

class LCContext c => TypeChecker c where
  typeFromContext :: c -> VarName -> Maybe Binding
  isCorrect :: c -> Bool
  typeOf :: c -> Either TypeError Type

instance TypeChecker (FullRefContext Term) where
  typeFromContext context name = liftM snd $ pickVar context name
  isCorrect c = isRight $ typeOf c

  typeOf (FullRefContext _ _ (TTrue _)) = return TyBool
  typeOf (FullRefContext _ _ (TFalse _)) = return TyBool
  typeOf (FullRefContext _ _ (TString _ _)) = return TyString
  typeOf (FullRefContext _ _ (TFloat _ _)) = return TyFloat
  typeOf (FullRefContext _ _ (TInt _ _)) = return TyInt
  typeOf (FullRefContext _ _ (TUnit _)) = return TyUnit
  typeOf (FullRefContext _ _ (TZero _)) = return TyNat

  typeOf (FullRefContext n s (TSucc info t)) = do
      ty <- typeOf $ FullRefContext n s t
      case ty of
          TyNat -> return TyNat
          ty -> Left $ TypeMissmatch info $ "argument of succ is not a natural number (" ++ show ty ++ ")"

  typeOf (FullRefContext n s (TPred info t)) = do
      ty <- typeOf $ FullRefContext n s t
      case ty of
         TyNat -> return TyNat
         ty -> Left $ TypeMissmatch info $ "argument of pred is not a natural number (" ++ show ty ++ ")"

  typeOf (FullRefContext n s (TIsZero info t)) = do
      ty <- typeOf $ FullRefContext n s t
      case ty of
        TyNat -> return TyBool
        ty -> Left $ TypeMissmatch info $ "argument of zero? is not a natural number (" ++ show ty ++ ")"

  typeOf (FullRefContext n s (TIf info t1 t2 t3)) = do
      ty1 <- typeOf $ FullRefContext n s t1
      ty2 <- typeOf $ FullRefContext n s t2
      ty3 <- typeOf $ FullRefContext n s t3
      case ty1 of
           TyBool -> if ty2 == ty3
                     then return ty2
                     else Left $ TypeMissmatch info $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")"
           ty -> Left $ TypeMissmatch info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty ++ ")"

  typeOf c@(FullRefContext n s v@(TVar info varname depth)) =
      let ty = typeFromContext c varname
      in case ty of
              Just (VarBind ty') -> return ty'
              Just x -> Left $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
              Nothing -> Left $ TypeMissmatch info $ "var type error"

  typeOf (FullRefContext n m (TApp info t1 t2)) = do
      ty1 <- typeOf $ FullRefContext n m $ t1
      ty2 <- typeOf $ FullRefContext n m $ t2
      case ty1 of
           (TyArrow ty1' ty2') -> if ty2 <: ty1'
                                  then return ty2'
                                  else Left $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
           TyBot -> return TyBot
           x -> Left $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

  typeOf c@(FullRefContext n m (TAbs _ name ty t)) = do
      let t' = bind (FullRefContext n m t) name (VarBind ty)
      ty' <- typeOf t'
      return $ TyArrow ty ty'

  typeOf (FullRefContext n s (TRef info t)) = do
      case typeOf $ FullRefContext n s t of
           Right x -> return $ TyRef x
           x -> x

  typeOf c@(FullRefContext n s (TDeref info t)) = do
      ty <- typeOf $ FullRefContext n s t
      case ty of
           TyRef x -> return x
           x -> Left $ TypeMissmatch info $ "incorect deref not reference type (" ++ show x ++ ")"

  typeOf (FullRefContext n s (TAssign info t1 t2)) = do
      ty1 <- typeOf $ FullRefContext n s t1
      ty2 <- typeOf $ FullRefContext n s t2
      case ty1 of
           (TyRef ty2) -> return TyUnit
           _           -> Left $ TypeMissmatch info $ "invalid assignment of " ++ show ty1 ++ " to " ++ show ty2

  typeOf (FullRefContext n m (TLoc _ location)) = do
      let t = lookup m location
      ty <- typeOf $ FullRefContext n m t
      return $ TyRef ty

  typeOf c@(FullRefContext n s (TLet info v t1 t2)) = do
      ty1 <- typeOf $ FullRefContext n s t1
      let context' = bind c v (VarBind ty1)
      ty2 <- typeOf $ context' `withTerm` t2
      return ty2

  typeOf c@(FullRefContext n m (TAscribe info t ty)) = do
      ty' <- typeOf $ c `withTerm` t
      if ty' <: ty
      then return ty
      else Left $ TypeMissmatch info "body of as-term does not have the expected type"

  typeOf c@(FullRefContext n m (TPair info t1 t2)) = do
      ty1 <- typeOf $ c `withTerm` t1
      ty2 <- typeOf $ c `withTerm` t2
      return $ TyProduct ty1 ty2

  typeOf c@(FullRefContext n m v@(TRecord info fields)) = do
      let f t = typeOf $ c `withTerm` t
      let tyFields = (\(k, v) -> (k, f v)) <$> fields
      let check = all (\(k, v) -> isRight v) tyFields
      let tys = fmap (\(k, (Right t)) -> (k,t)) tyFields
      let err = head $ fmap snd $ filter (\(_, v) -> isLeft v) tyFields
      if check
      then return $ TyRecord tys
      else err

  typeOf c@(FullRefContext n m (TLookup _ t (TInt info i))) =
      case (typeOf $ c `withTerm` t, i) of
           ((Right (TyProduct ty _)), 0) -> Right ty
           ((Right (TyProduct _ ty)), 1) -> Right ty
           ((Right (TyProduct _ ty)), _) -> Left $ TypeMissmatch info "invalid index for pair"
           (x, _) | isRight x -> Left $ TypeMissmatch info "invalid lookup operation"
           (x, _) -> x

  typeOf c@(FullRefContext n m (TLookup _ t (TKeyword info key))) =
      case typeOf $ c `withTerm` t of
           (Right (TyRecord fields)) -> case Prelude.lookup key fields of
                                             Just ty -> return ty
                                             _ -> Left $ TypeMissmatch info $ "invalid keyword " ++ show key ++ " for record " ++ (show $ c `withTerm` t)
           x | isRight x -> Left $ TypeMissmatch info "invalid lookup operation"
           x -> x

  typeOf c@(FullRefContext n m (TLookup info t k)) = do
      Left $ TypeMissmatch info "invalid lookup operation"

  typeOf c@(FullRefContext n m (TFix info t1)) = do
    tyT1 <- typeOf $ c `withTerm` t1
    case tyT1 of
         (TyArrow tyT11 tyT12) | tyT12 <: tyT11 -> return tyT12
         (TyArrow tyT11 tyT12) -> Left $ TypeMissmatch info  "result of body not compatible with domain"
         _ -> Left $ TypeMissmatch info  "arrow type expected"

