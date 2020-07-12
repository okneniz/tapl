module Language.TAPL.FullRef.TypeChecker where

import Prelude hiding (abs, succ, pred)
import Data.List (intercalate, nub, (\\))

import qualified Data.Map.Lazy as Map

import Control.Monad (liftM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Text.Parsec (SourcePos)

import Language.TAPL.FullRef.Types
import Language.TAPL.FullRef.Context

type Inferred a = ExceptT TypeError (State (LCNames, LCMemory)) a

data TypeError = TypeMissmatch SourcePos String

typeOf :: LCNames -> LCMemory -> Term -> Either String Type
typeOf names mem term =
    case evalState (runExceptT (infer term)) (names, mem) of
         Left x -> Left $ show x
         Right x -> return x

infer :: Term -> Inferred Type
infer (TTrue _) = return TyBool
infer (TFalse _) = return TyBool
infer (TString _ _) = return TyString
infer (TFloat _ _) = return TyFloat
infer (TInt _ _) = return TyInt
infer (TUnit _) = return TyUnit
infer (TZero _) = return TyNat

infer (TSucc info t) = do
  ty <- infer t
  case ty of
      TyNat -> return TyNat
      _ -> argumentError info TyNat ty

infer (TPred info t) = do
  ty <- infer t
  case ty of
     TyNat -> return TyNat
     _ -> argumentError info TyNat ty

infer (TIsZero info t) = do
  ty <- infer t
  case ty of
    TyNat -> return TyBool
    _ -> argumentError info TyNat ty

infer (TIf info t1 t2 t3) = do
  ty1 <- infer t1
  case ty1 of
       TyBool -> do
          ty2 <- infer t2
          ty3 <- infer t3
          if ty2 == ty3
          then return ty2
          else throwE $ TypeMissmatch info $ "branches of condition have different types (" ++ show t2 ++ " and " ++ show t3 ++ ")"
       _ -> throwE $ TypeMissmatch info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty1 ++ ")"

infer v@(TVar info varname _) = do
  (names, _) <- lift $ get
  case liftM snd $ pickVar names varname of
       Just (VarBind ty') -> return ty'
       Just x -> throwE $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show names ++ " " ++ show v ++ ")"
       Nothing -> throwE $ TypeMissmatch info $ "var type error"

infer (TApp info t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    case ty1 of
         (TyArrow ty1' ty2') | ty2 <: ty1' -> return ty2'
         (TyArrow ty1' _) -> throwE $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
         TyBot -> return TyBot
         _ -> throwE $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

infer (TAbs _ name ty t) = do
  (names, mem) <- lift $ get
  lift $ put $ (bind name (VarBind ty) names, mem)
  ty' <- infer t
  lift $ put (names, mem)
  return $ TyArrow ty ty'

infer (TPair _ t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    return $ TyProduct ty1 ty2

infer (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = do
            tyf <- infer v
            return (k, tyf)

infer (TLookup _ t (TInt info i)) = do
    ty <- infer t
    case (ty, i) of
         ((TyProduct x _), 0) -> return x
         ((TyProduct _ x), 1) -> return x
         ((TyProduct _ _), _) -> throwE $ TypeMissmatch info "invalid index for pair"
         (_, _)               -> throwE $ TypeMissmatch info "invalid lookup operation"

infer (TLookup _ t (TKeyword info key)) = do
    ty <- infer t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> throwE $ TypeMissmatch info $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         _ -> throwE $ TypeMissmatch info "invalid lookup operation"

infer (TLookup info _ _) = throwE $ TypeMissmatch info "invalid lookup operation"

infer (TLet _ v t1 t2) = do
    ty1 <- infer t1
    (names, mem) <- lift get
    lift $ put $ (bind v (VarBind ty1) names, mem)
    ty2 <- infer t2
    lift $ put (names, mem)
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
         (TyArrow _ _) -> throwE $ TypeMissmatch info  "result of body not compatible with domain"
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
                    (names, mem) <- lift $ get
                    lift $ put $ (bind varName (VarBind vty) names, mem)
                    ty <- infer t
                    lift $ put (names, mem)
                    return (caseName, ty)
--         _ -> throwE $ TypeMissmatch info $ "Invalid case statement"

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

infer (TRef _ t) = do
    ty <- infer t
    return $ TyRef ty

infer (TDeref info t) = do
    ty <- infer t
    case ty of
        TyRef x -> return x
        x -> throwE $ TypeMissmatch info $ "incorect deref not reference type (" ++ show x ++ ")"

infer (TAssign info t1 t2) = do
  ty1 <- infer t1
  ty2 <- infer t2
  case ty1 of
       (TyRef _) -> return TyUnit
       _         -> throwE $ TypeMissmatch info $ "invalid assignment of " ++ show ty1 ++ " to " ++ show ty2

infer (TLoc _ location) = do
  (_, mem) <- lift get
  let t = Language.TAPL.FullRef.Types.lookup mem location
  ty <- infer t
  return $ TyRef ty

argumentError :: SourcePos -> Type -> Type -> Inferred Type
argumentError info expected actual = throwE $ TypeMissmatch info message
    where message = "Argument error, expected " ++ show expected  ++ ". Got " ++ show actual ++ "."

instance Show TypeError where
    show (TypeMissmatch pos message) = message ++ " in " ++ show pos
