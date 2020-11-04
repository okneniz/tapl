module Language.TAPL.FullSimple.TypeChecker (typeOf) where

import Prelude hiding (abs, succ, pred)
import Data.List (tails, intercalate, all, (\\), sort)
import Text.Parsec (SourcePos)

import qualified Data.Map.Lazy as Map

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Pretty
import Language.TAPL.FullSimple.Context

data TypeError = TypeMissmatch SourcePos String

typeOf :: Term -> Eval Type
typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf p t1 t2 t3) = do
  ty1 <- typeOf t1
  unlessM (typeEq ty1 TyBool) (unexpectedType p TyBool ty1)
  ty2 <- typeOf t2
  ty3 <- typeOf t3
  unlessM (typeEq ty2 ty3) $ do
    ty2p <- prettifyType ty2
    ty3p <- prettifyType ty3
    typeError p $ "branches of condition have different types (" ++ show ty2p ++ " and " ++ show ty3p ++ ")"
  return ty2

typeOf (TCase p v branches) = do
    ty' <- simplifyType =<< typeOf v
    case ty' of
         TyVariant fields -> do
            when (not $ null invalidCaseBranches)
                 (typeError p $ "Invalid case branches : " ++ intercalate ", " invalidCaseBranches)

            when (not $ null absentCaseBranches)
                 (typeError p $ "Absent case branches : " ++ intercalate ", " absentCaseBranches)

            cases <- sequence $ fmap caseType $ Map.toList $ Map.intersectionWith (,) branches fields
            theSameTypes <- sequence $ [typeEq t1 t2 | (t1:ys) <- tails $ snd <$> cases, t2 <- ys]

            unless (all id theSameTypes)
                   (typeError p $ "Case branches have different types")

            return $ snd $ head cases

            where variantKeys = Map.keys fields
                  branchesKeys = Map.keys branches
                  invalidCaseBranches = branchesKeys \\ variantKeys
                  absentCaseBranches = variantKeys \\ branchesKeys
                  caseType (caseName, ((varName, t), vty)) =
                    withTmpStateT (addVar varName vty) $ do
                        ty <- typeOf t
                        return (caseName, ty)

         x -> typeError p $ "Invalid context for case statement " ++ show x

typeOf (TTag p key t tyT) = do
    tyT' <- simplifyType tyT
    case tyT' of
         TyVariant tys ->
            case Map.lookup key tys of
                 Just expected -> do
                    actual <- typeOf t
                    unlessM (typeEq actual expected) (unexpectedType p expected actual)
                    return tyT
                 _ -> typeError p $ "label " ++ key ++ " not found"
         _ -> typeError p $ "Annotation is not a variant type"

typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError p "var type error"

typeOf (TAbs _ x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

typeOf (TApp p t1 t2) = do
    ty1 <- simplifyType =<< typeOf t1
    ty2 <- simplifyType =<< typeOf t2
    case ty1 of
         (TyArrow ty1' ty2') -> do
            unlessM (typeEq ty2 ty1') $ do
                ty1p <- prettifyType ty1
                ty2p <- prettifyType ty2
                typeError p $ "incorrect application " ++ show ty2p ++ " to " ++ show ty1p
            return ty2'
         _ -> do
            ty1p <- prettifyType ty1
            typeError p $ "arrow type expected, insted" ++ show ty1p

typeOf (TLet _ x t1 t2) = do
    ty1 <- typeOf t1
    withTmpStateT (addVar x ty1) $ do
        ty2 <- typeOf t2
        return $ typeShift (-1) ty2

typeOf (TFix p t1) = do
    tyT1 <- simplifyType =<< typeOf t1
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            unlessM (typeEq tyT12 tyT11)
                    (typeError p  "result of body not compatible with domain")
            return tyT12
         _ -> typeError p  "arrow type expected"

typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit

typeOf (TAscribe p t ty) = do
    ty' <- typeOf t
    unlessM (typeEq ty' ty)
            (typeError p "body of as-term does not have the expected type")
    return ty

typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = (,) <$> return k <*> typeOf v

typeOf (TPair _ t1 t2) = TyProduct <$> (simplifyType =<< typeOf t1)
                                   <*> (simplifyType =<< typeOf t2)

typeOf (TProj p t key) = do
    ty <- simplifyType =<< typeOf t
    case (ty, key) of
         (TyRecord fields, _) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "label " ++ show key ++ " not found"
         ((TyProduct x _), "0") -> return x
         ((TyProduct _ x), "1") -> return x
         ((TyProduct _ _), _) -> typeError p "invalid index for pair"
         _ -> typeError p "expected record or pair types"

typeOf (TProj p _ _) = typeError p "invalid projection"
typeOf (TFloat _ _) = return TyFloat

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    unlessM (typeEq ty1 TyFloat) (unexpectedType p TyFloat ty1)
    ty2 <- typeOf t2
    unlessM (typeEq ty2 TyFloat) (unexpectedType p TyFloat ty2)
    return TyFloat

typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
  ty <- typeOf t
  unlessM (typeEq ty TyNat)(unexpectedType p TyNat ty)
  return TyNat

typeOf (TPred p t) = do
  ty <- typeOf t
  unlessM (typeEq ty TyNat)(unexpectedType p TyNat ty)
  return TyNat

typeOf (TIsZero p t) = do
  ty <- typeOf t
  unlessM (typeEq ty TyNat)(unexpectedType p TyNat ty)
  return TyBool

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

unexpectedType :: SourcePos -> Type -> Type -> Eval a
unexpectedType p expected actual = do
    tyE <- prettifyType expected
    tyA <- prettifyType actual
    typeError p $ "expected type " ++ show tyE ++ ", actual " ++ show tyA

instance Show TypeError where
    show (TypeMissmatch p message) = show p ++ ":" ++ message

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    ty1' <- simplifyType ty1
    ty2' <- simplifyType ty2
    n <- get
    case (ty1', ty2') of
      (TyString, TyString) -> return True
      (TyUnit, TyUnit) -> return True
      ((TyID x), (TyID y)) -> return $ x == y
      (TyFloat, TyFloat) -> return True

      (TyVar _ i, _) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x ty2'
                _ -> return False

      (_, TyVar _ i) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x ty1'
                _ -> return False

      (TyVar _ i, TyVar _ j) | i == j -> return True
      (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2

      (TyBool, TyBool) -> return True
      (TyNat, TyNat) -> return True

      (TyProduct tyS1 tyS2, TyProduct tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2

      (TyRecord f1, TyRecord f2) | (Map.keys f1) /= (Map.keys f2) -> return False
      (TyRecord f1, TyRecord f2) ->
        all (id) <$> sequence (uncurry typeEq <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

      (TyVariant f1, TyVariant f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
      (TyVariant f1, TyVariant f2) ->
        all (id) <$> sequence (uncurry typeEq <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

      _ -> return False
