{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullSimple.TypeChecker (typeOf) where

import Prelude hiding (abs, succ, pred)

import Data.List (intercalate, all, nub, (\\), find, sortBy)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

import qualified Data.Map.Strict as Map

import Control.Monad (liftM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import TAPL.FullSimple.Types
import TAPL.FullSimple.Context

type Inferred a = ExceptT TypeError (State LCNames) a

data TypeError = TypeMissmatch Info String

typeOf :: LCNames -> Term -> Either String Type
typeOf names term =
    case evalState (runExceptT (infer term)) names of
         Left x -> Left $ show x
         Right x -> return x

infer :: Term -> Inferred Type
infer (TTrue _) = return TyBool
infer (TFalse _) = return TyBool
infer (TString _ _) = return TyString
infer (TUnit _) = return TyUnit
infer (TZero _) = return TyNat
infer (TInt _ _) = return TyInt

infer (TSucc info t) = do
  ty <- infer t
  case ty of
      TyNat -> return TyNat
      ty -> argumentError info TyNat ty

infer (TPred info t) = do
  ty <- infer t
  case ty of
     TyNat -> return TyNat
     ty -> argumentError info TyNat ty

infer (TIsZero info t) = do
  ty <- infer t
  case ty of
    TyNat -> return TyBool
    ty -> argumentError info TyNat ty

infer (TIf info t1 t2 t3) = do
  ty1 <- infer t1
  names <- lift $ get
  case ty1 of
       TyBool -> do
          ty2 <- infer t2
          ty3 <- infer t3
          if ty2 == ty3
          then return ty2
          else throwE $ TypeMissmatch info $ "branches of condition have different types (" ++ show t2 ++ " and " ++ show t3 ++ ")"
       ty -> throwE $ TypeMissmatch info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty ++ ")"

infer v@(TVar info varname depth) = do
  names <- lift $ get
  case liftM snd $ pickVar names varname of
       Just (VarBind ty') -> return ty'
       Just x -> throwE $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show names ++ " " ++ show v ++ ")"
       Nothing -> throwE $ TypeMissmatch info $ "var type error"

infer (TApp info t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    case ty1 of
         (TyArrow ty1' ty2') | ty2 <: ty1' -> return ty2'
         (TyArrow ty1' ty2') -> throwE $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
         TyBot -> return TyBot
         x -> throwE $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

infer c@(TAbs _ name ty t) = do
  names <- lift $ get
  lift $ modify $ bind name (VarBind ty)
  ty' <- infer t
  lift $ put names
  return $ TyArrow ty ty'

infer (TFloat info x) = return TyFloat

infer (TPair info t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    return $ TyProduct ty1 ty2

infer (TRecord info fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = do
            tyf <- infer v
            return (k, tyf)

infer (TLookup _ t (TInt info i)) = do
    ty <- infer t
    case (ty, i) of
         ((TyProduct ty _), 0) -> return ty
         ((TyProduct _ ty), 1) -> return ty
         ((TyProduct _ ty), _) -> throwE $ TypeMissmatch info "invalid index for pair"
         (x, _)                -> throwE $ TypeMissmatch info "invalid lookup operation"

infer (TLookup _ t (TKeyword info key)) = do
    ty <- infer t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just ty -> return ty
                 _ -> throwE $ TypeMissmatch info $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         _ -> throwE $ TypeMissmatch info "invalid lookup operation"

infer (TLookup info t k) = throwE $ TypeMissmatch info "invalid lookup operation"

infer (TLet info v t1 t2) = do
    ty1 <- infer t1
    lift $ modify $ bind v (VarBind ty1)
    ty2 <- infer t2
    return ty2

infer (TAscribe info t ty) = do
    ty' <- infer t
    if ty' <: ty
    then return ty
    else throwE $ TypeMissmatch info "body of as-term does not have the expected type"

infer (TFix info t1) = do
    tyT1 <- infer t1
    case tyT1 of
         (TyArrow tyT11 tyT12) | tyT12 <: tyT11 -> return tyT12
         (TyArrow tyT11 tyT12) -> throwE $ TypeMissmatch info  "result of body not compatible with domain"
         _ -> throwE $ TypeMissmatch info  "arrow type expected"

infer (TCase info v@(TTag _ key _ _) branches) = do
    ty' <- infer v
    case ty' of
         TyVariant fields -> do
            when (not $ null invalidCaseBranches)
                 (throwE $ TypeMissmatch info $ "Invalid case branches : " ++ intercalate ", " invalidCaseBranches)

            when (not $ null absentCaseBranches)
                 (throwE $ TypeMissmatch info $ "Absent case branches : " ++ intercalate ", " absentCaseBranches)

            cases <- sequence $ fmap caseType $ Map.toList $ Map.intersectionWith (,) branches fields

            let casesTypes' = nub $ snd <$> cases
            when (length casesTypes' /= 1)
                 (throwE $ TypeMissmatch info $ "Case branches have different types : " ++ intercalate ", " (show <$> casesTypes'))

            case (Map.lookup key $ Map.fromList cases) of
                 Just vty' -> return vty'
                 _ -> throwE $ TypeMissmatch info $ "Variant with key " ++ show key ++ " not found."

            where variantKeys = Map.keys fields
                  branchesKeys = Map.keys branches
                  invalidCaseBranches = branchesKeys \\ variantKeys
                  absentCaseBranches = variantKeys \\ branchesKeys
                  caseType (caseName, ((varName, t), vty)) = do
                    lift $ modify $ bind varName (VarBind vty)
                    ty <- infer t
                    return (caseName, ty)

infer (TTag info key t ty) = do
    ty' <- infer t
    case ty of
         TyVariant tys ->
            case Map.lookup key tys of
                 Just x -> if x == ty'
                           then return ty
                           else throwE $ TypeMissmatch info $ "field does not have expected type"
                 _ -> throwE $ TypeMissmatch info $ "label " ++ key ++ " not found"
         _ -> throwE $ TypeMissmatch info $ "Annotation is not a variant type"

argumentError :: Info -> Type -> Type -> Inferred Type
argumentError info expected actual = throwE $ TypeMissmatch info message
    where message = "Argument error, expected " ++ show expected  ++ ". Got " ++ show actual ++ "."

instance Show TypeError where
    show (TypeMissmatch info message) = message ++ " in " ++ (show $ row info) ++ ":" ++ (show $ column info)

instance Eq Type where
  TyBool == TyBool = True
  TyString == TyString = True
  TyUnit == TyUnit = True
  TyNat == TyNat = True
  TyFloat == TyFloat = True
  TyInt == TyInt = True
  (TyID x) == (TyID y) = x == y
  TyTop == TyTop = True
  TyBot == TyBot = True
  (TyArrow tys1 tys2) == (TyArrow tyt1 tyt2) = (tys1 == tyt1) && (tys2 == tyt2)
  (TyProduct tyT1 tyT2) == (TyProduct tyT1' tyT2') = (tyT1 == tyT2) && (tyT1' == tyT2')
  (TyRecord tys1) == (TyRecord tys2) = tys1 == tys2
  (TyVariant tys1) == (TyVariant tys2) = tys1 == tys2
  _ == _ = False

(<:) :: Type -> Type -> Bool
_ <: TyTop = True
TyBot <: _ = True
(TyArrow tys1 tys2) <: (TyArrow tyt1 tyt2) = (tyt1 <: tys1) && (tys2 <: tyt2)
(TyProduct tyS1 tyS2) <: (TyProduct tyT1 tyT2) = (tyS1 <: tyT1) && (tyS2 <: tyT2)

(TyRecord ty1) <: (TyRecord ty2) =
    all f $ Map.elems $ Map.intersectionWith (,) ty1 $ Map.filterWithKey (\k _ -> Map.member k ty1) ty2
    where f (ty1', ty2') = ty1' <: ty2'

(TyVariant ty1) <: (TyVariant ty2) =
    all f $ Map.elems $ Map.intersectionWith (,) ty1 $ Map.filterWithKey (\k _ -> Map.member k ty1) ty2
    where f (ty1', ty2') = ty1' <: ty2'

x <: y | x == y = True
x <: y = False
