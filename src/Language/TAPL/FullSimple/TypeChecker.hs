module Language.TAPL.FullSimple.TypeChecker (typeOf) where

import Prelude hiding (abs, succ, pred)
import Data.List (intercalate, all, nub, (\\))

import qualified Data.Map.Strict as Map

import Control.Monad (liftM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Context

data TypeError = TypeMissmatch Info String

typeOf :: Term -> Eval Type
typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool
typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit
typeOf (TZero _) = return TyNat
typeOf (TInt _ _) = return TyInt

typeOf (TSucc info t) = do
  ty <- typeOf t
  case ty of
      TyNat -> return TyNat
      _ -> argumentError info TyNat ty

typeOf (TPred info t) = do
  ty <- typeOf t
  case ty of
     TyNat -> return TyNat
     _ -> argumentError info TyNat ty

typeOf (TIsZero info t) = do
  ty <- typeOf t
  case ty of
    TyNat -> return TyBool
    _ -> argumentError info TyNat ty

typeOf (TIf info t1 t2 t3) = do
  ty1 <- typeOf t1
  case ty1 of
       TyBool -> do
          ty2 <- typeOf t2
          ty3 <- typeOf t3
          if ty2 == ty3
          then return ty2
          else typeError info $ "branches of condition have different types (" ++ show t2 ++ " and " ++ show t3 ++ ")"
       _ -> typeError info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty1 ++ ")"

typeOf v@(TVar info varname _) = do
  names <- get
  case liftM snd $ pickVar names varname of
       Just (VarBind ty') -> return ty'
       Just x -> typeError info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show names ++ " " ++ show v ++ ")"
       Nothing -> typeError info $ "var type error"

typeOf (TApp info t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    case ty1 of
         (TyArrow ty1' ty2') | ty2 <: ty1' -> return ty2'
         (TyArrow ty1' _) -> typeError info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
         TyBot -> return TyBot
         _ -> typeError info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

typeOf (TAbs _ name ty t) = do
  names <- get
  modify $ bind name (VarBind ty)
  ty' <- typeOf t
  put names
  return $ TyArrow ty ty'

typeOf (TFloat _ _) = return TyFloat

typeOf (TPair _ t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    return $ TyProduct ty1 ty2

typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = do
            tyf <- typeOf v
            return (k, tyf)

typeOf (TLookup _ t (TInt info i)) = do
    ty <- typeOf t
    case (ty, i) of
         ((TyProduct x _), 0) -> return x
         ((TyProduct _ x), 1) -> return x
         ((TyProduct _ _), _) -> typeError info "invalid index for pair"
         (_, _)               -> typeError info "invalid lookup operation"

typeOf (TLookup _ t (TKeyword info key)) = do
    ty <- typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError info $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         _ -> typeError info "invalid lookup operation"

typeOf (TLookup info _ _) = typeError info "invalid lookup operation"

typeOf (TLet _ v t1 t2) = do
    ty1 <- typeOf t1
    modify $ bind v (VarBind ty1)
    ty2 <- typeOf t2
    -- TODO : recover state
    return ty2

typeOf (TAscribe info t ty) = do
    ty' <- typeOf t
    if ty' <: ty
    then return ty
    else typeError info "body of as-term does not have the expected type"

typeOf (TFix info t1) = do
    tyT1 <- typeOf t1
    case tyT1 of
         (TyArrow tyT11 tyT12) | tyT12 <: tyT11 -> return tyT12
         (TyArrow _ _) -> typeError info  "result of body not compatible with domain"
         _ -> typeError info  "arrow type expected"

typeOf (TCase info v@(TTag _ key _ _) branches) = do
    ty' <- typeOf v
    case ty' of
         TyVariant fields -> do
            when (not $ null invalidCaseBranches)
                 (typeError info $ "Invalid case branches : " ++ intercalate ", " invalidCaseBranches)

            when (not $ null absentCaseBranches)
                 (typeError info $ "Absent case branches : " ++ intercalate ", " absentCaseBranches)

            cases <- sequence $ fmap caseType $ Map.toList $ Map.intersectionWith (,) branches fields

            let casesTypes' = nub $ snd <$> cases
            when (length casesTypes' /= 1)
                 (typeError info $ "Case branches have different types : " ++ intercalate ", " (show <$> casesTypes'))

            case (Map.lookup key $ Map.fromList cases) of
                 Just vty' -> return vty'
                 _ -> typeError info $ "Variant with key " ++ show key ++ " not found."

            where variantKeys = Map.keys fields
                  branchesKeys = Map.keys branches
                  invalidCaseBranches = branchesKeys \\ variantKeys
                  absentCaseBranches = variantKeys \\ branchesKeys
                  caseType (caseName, ((varName, t), vty)) = do
                    names <- get
                    modify $ bind varName (VarBind vty)
                    ty <- typeOf t
                    put names
                    return (caseName, ty)
         x -> (typeError info $ "Invalid context for case statement " ++ show x)

typeOf (TTag info key t ty) = do
    ty' <- typeOf t
    case ty of
         TyVariant tys ->
            case Map.lookup key tys of
                 Just x -> if x == ty'
                           then return ty
                           else typeError info $ "field does not have expected type"
                 _ -> typeError info $ "label " ++ key ++ " not found"
         _ -> typeError info $ "Annotation is not a variant type"

typeError :: Info -> String -> Eval a
typeError info message = lift $ throwE $ show info ++ ":" ++ message

argumentError :: Info -> Type -> Type -> Eval Type
argumentError info expected actual = typeError info message
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
_ <: _ = False
