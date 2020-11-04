module Language.TAPL.FullOmega.TypeChecker (typeOf, kindOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate, sort)
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Control.Monad (when, unless, liftM, liftM2, when, unless, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.FullOmega.Pretty (render, renderType)

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Context
import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullOmega.Types
import Language.TAPL.FullOmega.Context

typeOf :: Term -> Eval Type
typeOf (TAscribe p t1 tyT) = do
    unlessM (isStar p tyT) (typeError p "Kind * expected")
    ty1 <- typeOf t1
    unlessM (typeEq p ty1 tyT) (typeError p "body of as-term does not have the expected type")
    return tyT

typeOf (TVar p v _) = do
    n <- getNames
    b <- getBinding p n v
    case b of
         (Just (VarBind ty))-> return ty
         (Just x)-> typeError p $ "wrong kind of binding for variable " ++ show x
         Nothing -> typeError p "var type error"

typeOf (TAbs p x tyT1 t2) = do
    unlessM (isStar p tyT1) (typeError p "Kind * expected")
    withTmpStateT (\s -> s { names = addVar x tyT1 (names s)}) $ do
        tyT2 <- typeOf t2
        TyArrow tyT1 <$> typeShift p (-1) tyT2

typeOf r@(TApp p t1 t2) = do
    tyT1 <- simplifyType p =<< typeOf t1
    tyT2 <- typeOf t2
    n <- getNames
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            tyT22 <- simplifyType p tyT2
            unlessM (typeEq p tyT2 tyT11) (typeError p $ "parameter type missmatch " ++ show tyT2 ++ "  " ++ show tyT11)
            return tyT12
         x -> typeError p $ "arrow type expected " ++ show x ++ " : " ++ show n

typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit
typeOf (TRef _ t) = TyRef <$> typeOf t
typeOf (TLoc p _) = typeError p "locations are not supposed to occur in source programs!"

typeOf (TDeref p t1) = do
    ty1 <- simplifyType p =<< typeOf t1
    case ty1 of
         TyRef tyT1 -> return tyT1
         _ -> typeError p "argument of ! is not a Ref"

typeOf (TAssign p t1 t2) = do
    ty1 <- simplifyType p =<< typeOf t1
    ty2 <- typeOf t2
    case ty1 of
         (TyRef tyT1) -> do
            unlessM (typeEq p ty2 tyT1) (typeError p $ "arguments of := are incompatible " ++ show ty2 ++ " -> " ++ show tyT1)
            return TyUnit
         _ -> typeError p "first argument of := is not a Ref"

typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = (,) k <$> typeOf v

typeOf s@(TProj p t key) = do
    ty <- simplifyType p =<< typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         x -> typeError p $ "Expected record type: " ++ show s

typeOf (TProj p _ _) = typeError p "invalid lookup operation"
typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (typeEq p ty1 TyBool) (typeError p $ "guard of conditional not a boolean")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    unlessM (typeEq p ty2 ty3) (typeError p $ "arms of conditional have different types")
    return ty2

typeOf (TLet p x t1 t2) = do
    n <- getNames
    ty1 <- typeOf t1
    withTmpStateT (\s -> s { names = addVar x ty1 (names s)}) $ typeShift p (-1) =<< typeOf t2

typeOf (TFloat _ _) = return TyFloat

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    unlessM (typeEq p ty1 TyFloat) (argumentError p TyFloat ty1)
    ty2 <- typeOf t2
    unlessM (typeEq p ty2 TyFloat) (argumentError p TyFloat ty2)
    return TyFloat

typeOf (TFix p t1) = do
    tyT1 <- simplifyType p =<< typeOf t1
    case tyT1 of
        (TyArrow tyT11 tyT12) -> do
            unlessM (typeEq p tyT12 tyT11) (typeError p "result of body not compatible with domain")
            return tyT12
        _ -> typeError p  "arrow type expected"

typeOf (TTAbs p x k t) = withTmpStateT (\s -> s { names = addTypeVar x k (names s)}) $ TyAll x k <$> typeOf t

typeOf (TTApp p t1 tyT2) = do
    knKT2 <- kindOf p tyT2
    tyT1 <- simplifyType p =<< typeOf t1
    case tyT1 of
        TyAll _ knK11 tyT12 | knK11 /= knKT2 -> typeError p "Type argument has wrong kind"
        TyAll _ knK11 tyT12 -> typeSubstitutionTop p tyT2 tyT12
        _ -> typeError p "universal type expected"

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

typeOf (TPack p tyT1 t2 tyT) = do
    unlessM (isStar p tyT) (typeError p "Kind * expected")
    tyTT <- simplifyType p tyT
    case tyTT of
         (TySome tyY k tyT2) -> do
            kT1 <- kindOf p tyT1
            unless (kT1 == k) (typeError p "type component does not have expected kind")
            tyU <- typeOf t2
            tyU' <- typeSubstitutionTop p tyT1 tyT2
            unlessM (typeEq p tyU tyU')
                    (typeError p $ "doesn\'t match declared type : " ++ show tyU ++ " and " ++ show tyU')
            return tyT

typeOf (TUnpack p tyX x t1 t2) = do
    tyT1 <- simplifyType p =<< typeOf t1
    case tyT1 of
         (TySome tyY k tyT11) -> do
            withTmpStateT (\s -> s { names = addTypeVar tyX k (names s) }) $ do
                withTmpStateT (\s -> s { names = addVar x tyT11 (names s) }) $ do
                    typeShift p (-2) =<< typeOf t2
         _ -> typeError p "existential type expected"

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
    where message = "Argument error, expected " ++ show expected  ++ ". Got " ++ show actual ++ "."

isStar :: SourcePos -> Type -> Eval Bool
isStar p ty = (==) Star <$> kindOf p ty

getKind :: SourcePos -> VarName -> Eval Kind
getKind p v = do
    x <- flip(getBinding p) v =<< getNames
    case x of
        (Just (TypeVarBind k)) -> return k
        (Just (TypeAddBind _ (Just k))) -> return k
        (Just (TypeAddBind _ Nothing)) -> lift $ throwE $ "No kind recorded for variable " ++ show v ++ " : " ++ show x
        _ -> lift $ throwE $ "Wrong kind of binding for variable  " ++ show v ++ " : " ++ show x

kindOf :: SourcePos -> Type -> Eval Kind
kindOf p (TyArrow tyT1 tyT2) = do
    unlessM (isStar p tyT1) (typeError p "star kind expected")
    unlessM (isStar p tyT2) (typeError p "star kind expected")
    return Star

kindOf p (TyVar i _) = getKind p i
kindOf p (TyRecord fields) = do
    unlessM(all (id) <$> sequence (isStar p <$> (Map.elems fields))) (typeError p "Kind * expected")
    return Star

kindOf p (TyAll tyX knK1 tyT2) = do
    withTmpStateT (\s -> s { names = addTypeVar tyX knK1 (names s) }) $ do
        unlessM (isStar p tyT2) (typeError p "star kind expected")
        return Star

kindOf p (TyAbs tyX knK1 tyT2) = do
    withTmpStateT (\s -> s { names = addTypeVar tyX knK1 (names s) }) $ do
        Arrow knK1 <$> kindOf p tyT2

kindOf p x@(TyApp tyT1 tyT2) = do
    knK1 <- kindOf p tyT1
    knK2 <- kindOf p tyT2
    names <- getNames
    case knK1 of
         (Arrow knK11 knK12) | knK2 == knK11 -> return knK12
         (Arrow knK11 knK12) -> typeError p "parameter kind mismatch"
         _ -> typeError p $ "arrow kind expected " ++ show x ++ " - " ++ show knK1 ++ show knK2 ++ " : " ++ show names

kindOf p (TySome tyX knK tyT2) = do
    withTmpStateT (\s -> s { names = addTypeVar tyX knK (names s) }) $ do
        unlessM (isStar p tyT2) (typeError p "star kind expected")
        return Star

kindOf _ _ = return Star

typeEq :: SourcePos -> Type -> Type -> Eval Bool
typeEq p t1 t2 = do
    tyS <- simplifyType p t1
    tyT <- simplifyType p t2
    n <- getNames
    case (tyS, tyT) of
      (TyString, TyString) -> return True
      (TyID x, TyID y) -> return $ x == y
      (TyUnit, TyUnit) -> return True
      (TyRef t1, TyRef t2) -> typeEq p t1 t2
      (TyFloat, TyFloat) -> return True

      (TyVar i _, TyVar j _) -> do
        bi <- isTypeAdd p n i
        bj <- isTypeAdd p n j
        case (tyS, tyT) of
             (_, _) | bi -> do
                m <- getTypeAbb p n i
                case m of
                     Just x -> typeEq p x tyT
                     _ -> return False

             (_, _) | bj -> do
                m <- getTypeAbb p n j
                case m of
                     Just x -> typeEq p x tyS
                     _ -> return False

             (TyVar i _, TyVar j _) -> return $ i == j

      (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2
      (TyBool, TyBool) -> return True
      (TyNat, TyNat) -> return True

      (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
      (TyRecord f1, TyRecord f2) ->
        all (id) <$> sequence (uncurry (typeEq p) <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

      (TySome x k1 ty1, TySome _ k2 ty2) | k1 == k2 ->
        withTmpStateT (\s -> s { names = addName x (names s) }) $ typeEq p ty1 ty2
      (TySome x k1 ty1, TySome _ k2 ty2) -> return False

      (TyAll x k1 ty1, TyAll _ k2 ty2) | k1 == k2 ->
        withTmpStateT (\s -> s { names = addName x (names s) }) $ typeEq p ty1 ty2
      (TyAll x k1 ty1, TyAll _ k2 ty2) -> return False

      (TyAbs x k1 ty1, TyAbs _ k2 ty2) | k1 == k2 ->
        withTmpStateT (\s -> s { names = addName x (names s) }) $ typeEq p ty1 ty2
      (TyAbs x k1 ty1, TyAbs _ k2 ty2) -> return False

      (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2
      _ -> return False

computeType :: SourcePos -> Type -> Eval (Maybe Type)
computeType p (TyApp (TyAbs _ _ ty1) ty2) = Just <$> typeSubstitutionTop p ty2 ty1
computeType p (TyVar i _) = do
    n <- getNames
    x <- isTypeAdd p n i
    if x
    then getTypeAbb p n i
    else return Nothing

computeType _ _ = return Nothing

simplifyType :: SourcePos -> Type -> Eval Type
simplifyType p z@(TyApp ty1 ty2) = do
    tyX <- flip(TyApp) ty2 <$> simplifyType p ty1
    n <- computeType p tyX
    case n of
         Just x -> simplifyType p x
         _ -> return tyX

simplifyType p ty = do
    n <- computeType p ty
    case n of
         Just x -> simplifyType p x
         _ -> return ty
