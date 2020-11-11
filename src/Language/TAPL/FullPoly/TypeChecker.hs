module Language.TAPL.FullPoly.TypeChecker (typeOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate, sort)

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT, nvm)
import Language.TAPL.FullPoly.Types
import Language.TAPL.FullPoly.Context

typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- get
    z <- getBinding p n v
    case z of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" <> show x <> " " <> show n <> " " <> show v <> ")"
         Nothing -> typeError p "var type error"

typeOf (TAbs p x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        TyArrow tyT1 <$> typeShift p (-1) tyT2

typeOf r@(TApp p t1 t2) = do
    tyT1 <- simplifyType p =<< typeOf t1
    tyT2 <- typeOf t2
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            x <- typeEq p tyT2 tyT11
            if x
            then return tyT12
            else typeError p $ "incorrect application of abstraction " <> show tyT2
         _ -> typeError p $ "incorrect application " <> show tyT1 <> " and " <> show tyT2

typeOf (TLet p x t1 t2) = do
    ty1 <- typeOf t1
    withTmpStateT (addVar x ty1) $ do
        ty2 <- typeOf t2
        typeShift p (-1) ty2

typeOf (TFix p t1) = do
    tyT1 <- typeOf t1
    tyT1' <- simplifyType p tyT1
    case tyT1' of
        (TyArrow tyT11 tyT12) -> do
            unlessM (typeEq p tyT12 tyT11) (typeError p $ "result of body not compatible with domain " <> show tyT11 <> " and " <> show tyT12)
            return tyT12
        _ -> typeError p  "arrow type expected"

typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit

typeOf (TAscribe p t1 ty) = do
    ty1 <- typeOf t1
    unlessM (typeEq p ty ty1) (typeError p "body of as-term does not have the expected type")
    return ty

typeOf (TRecord _ fields) = do
    tys <- mapM tyField (Map.toList fields)
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = ((,) k) <$> typeOf v

typeOf (TProj p t key) = do
    ty <- simplifyType p =<< typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "invalid keyword " <> show key <> " for record " <> (show t)
         _ -> typeError p "invalid lookup operation"

typeOf (TProj p _ _) = typeError p "invalid lookup operation"

typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (typeEq p ty1 TyBool)
            (typeError p $ "guard of condition have not a " <> show TyBool <>  " type (" <> show ty1 <> ")")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    unlessM (typeEq p ty2 ty3)
           (typeError p $ "branches of condition have different types (" <> show ty2 <> " and " <> show ty3 <> ")")
    return ty2

typeOf (TFloat _ _) = return TyFloat

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    unlessM (typeEq p ty1 TyFloat) (argumentError p TyFloat ty1)
    unlessM (typeEq p ty2 TyFloat) (argumentError p TyFloat ty2)
    return TyFloat

typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
    ty <- typeOf t
    unlessM (typeEq p ty TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TPred p t) = do
    ty <- typeOf t
    unlessM (typeEq p ty TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TIsZero p t) = do
    ty <- typeOf t
    unlessM (typeEq p ty TyNat) (argumentError p TyNat ty)
    return TyBool

typeOf x@(TPack p tyT1 t2 tyT) = do
    ty <- simplifyType p tyT
    case ty of
        TySome tyY tyT2 -> do
            tyU <- typeOf t2
            tyU' <- typeSubstitutionTop p tyT1 tyT2
            unlessM (typeEq p tyU tyU') (typeError p $ "doesn\'t match declared type ")
            return tyT
        _ -> typeError p "existential type expected"

typeOf (TUnpack p tyX x t1 t2) = do
    tyT1 <- simplifyType p =<< typeOf t1
    case tyT1 of
        TySome tyY tyT11 -> do
            withTmpStateT (addTypeVar tyX) $ do
                withTmpStateT (addVar x tyT11) $ do
                    typeShift p (-2) =<< typeOf t2
        _ ->  typeError p "existential type expected"

typeOf (TTAbs p tyX t2) = do
    withTmpStateT (addTypeVar tyX) $ do
        TyAll tyX <$> typeOf t2

typeOf (TTApp p t1 tyT2) = do
    tyT1 <- simplifyType p =<< typeOf t1
    case tyT1 of
        TyAll _ tyT12 -> typeSubstitutionTop p tyT2 tyT12
        _ -> typeError p "universal type expected"

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
    where message = "Argument error, expected " <> show expected  <> ". Got " <> show actual <> "."

typeEq :: SourcePos -> Type -> Type -> Eval Bool
typeEq p ty1 ty2 = do
    ty1' <- simplifyType p ty1
    ty2' <- simplifyType p ty2
    n <- get
    case (ty1', ty2') of
      (TyString, TyString) -> return True
      (TyUnit, TyUnit) -> return True
      (TyID x, TyID y) -> return $ x == y
      (TyFloat, TyFloat) -> return True

      (TyVar i _, TyVar j _) -> do
         bi <- isTypeAdd p n i
         bj <- isTypeAdd p n j
         case (ty1', ty2') of
              (_, _) | bi -> do
                 m <- getTypeAbb p n i
                 case m of
                      Just x -> typeEq p x ty2'
                      _ -> return False

              (_, _) | bj -> do
                 m <- getTypeAbb p n j
                 case m of
                      Just x -> typeEq p x ty1'
                      _ -> return False

              (TyVar i _, TyVar j _) -> return $ i == j

      (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2
      (TyBool, TyBool) -> return True
      (TyNat, TyNat) -> return True

      (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
      (TyRecord f1, TyRecord f2) -> all (id) <$> sequence (uncurry (typeEq p) <$> Map.elems (Map.intersectionWith (,) f1 f2))

      (TySome tyX1 tyS2, TySome _ tyT2) -> withTmpStateT (addName tyX1) (typeEq p tyS2 tyT2)
      (TyAll tyX1 tyS2, TyAll _ tyT2) -> withTmpStateT (addName tyX1) (typeEq p tyS2 tyT2)
      _ -> return False

computeType :: SourcePos -> Type -> Eval (Maybe Type)
computeType p (TyVar i _) = do
    n <- get
    x <- isTypeAdd p n i
    if x
    then getTypeAbb p n i
    else nvm

computeType _ _ = nvm

simplifyType :: SourcePos -> Type -> Eval Type
simplifyType p ty = do
    n <- computeType p ty
    case n of
         Just x -> simplifyType p x
         _ -> return ty
