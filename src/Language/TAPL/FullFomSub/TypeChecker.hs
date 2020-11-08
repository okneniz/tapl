module Language.TAPL.FullFomSub.TypeChecker (typeOf, kindOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate, sort)
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.FullFomSub.Pretty (render, renderType)

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Context
import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullFomSub.Types
import Language.TAPL.FullFomSub.Context

typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- get
    b <- getBinding p n v
    case b of
         (Just (VarBind ty))-> return ty
         (Just x)-> typeError p $ "wrong kind of binding for variable " <> show x
         Nothing -> typeError p "var type error"

typeOf (TAbs p x tyT1 t2) = do
    unlessM (isStar p tyT1) (typeError p "Kind * expected")
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        TyArrow tyT1 <$> typeShift p (-1) tyT2

typeOf r@(TApp p t1 t2) = do
    tyT1 <- lcst p =<< typeOf t1
    tyT2 <- typeOf t2
    n <- get
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            tyT22 <- simplifyType p tyT2
            unlessM (isSubtype p tyT2 tyT11) (typeError p $ "parameter type missmatch " <> show tyT2 <> "  " <> show tyT11)
            return tyT12
         x -> typeError p $ "arrow type expected " <> show x <> " : " <> show n

typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (isSubtype p ty1 TyBool) (typeError p $ "guard of conditional not a boolean")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    joinTypes p ty2 ty3

typeOf (TRecord _ fields) = do
    tys <- mapM tyField (Map.toList fields)
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = (,) k <$> typeOf v

typeOf s@(TProj p t key) = do
    ty <- lcst p =<< typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "invalid keyword " <> show key <> " for record " <> (show t)
         x -> typeError p $ "Expected record type: " <> show s

typeOf (TLet p x t1 t2) = do
    n <- get
    ty1 <- typeOf t1
    withTmpStateT (addVar x ty1) $ typeShift p (-1) =<< typeOf t2

typeOf (TFix p t1) = do
    tyT1 <- lcst p =<< typeOf t1
    case tyT1 of
        (TyArrow tyT11 tyT12) -> do
            unlessM (isSubtype p tyT12 tyT11) (typeError p "result of body not compatible with domain")
            return tyT12
        _ -> typeError p  "arrow type expected"

typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit

typeOf (TAscribe p t1 tyT) = do
    unlessM (isStar p tyT) (typeError p "Kind * expected")
    ty1 <- typeOf t1
    unlessM (isSubtype p ty1 tyT) (typeError p "body of as-term does not have the expected type")
    return tyT

typeOf (TFloat _ _) = return TyFloat

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    unlessM (isSubtype p ty1 TyFloat) (argumentError p TyFloat ty1)
    ty2 <- typeOf t2
    unlessM (isSubtype p ty2 TyFloat) (argumentError p TyFloat ty2)
    return TyFloat

typeOf (TTAbs p x ty t) = withTmpStateT (addTypeVar x ty) $ TyAll x ty <$> typeOf t

typeOf (TTApp p t1 tyT2) = do
    tyT1 <- lcst p =<< typeOf t1
    case tyT1 of
        TyAll _ tyT11 tyT12 -> do
            unlessM (isSubtype p tyT2 tyT11) (typeError p "type parameter type mismatch")
            typeSubstitutionTop p tyT2 tyT12
        _ -> typeError p "universal type expected"

typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
    ty <- typeOf t
    unlessM (isSubtype p ty TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TPred p t) = do
    ty <- typeOf t
    unlessM (isSubtype p ty TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TIsZero p t) = do
    ty <- typeOf t
    unlessM (isSubtype p ty TyNat) (argumentError p TyNat ty)
    return TyBool

typeOf (TPack p tyT1 t2 tyT) = do
    unlessM (isStar p tyT) (typeError p "Kind * expected")
    tyTT <- simplifyType p tyT
    case tyTT of
         (TySome tyY tyBound tyT2) -> do
            unlessM (isSubtype p tyT1 tyBound) (typeError p "hidden type not a subtype of bound")
            tyU <- typeOf t2
            tyU' <- typeSubstitutionTop p tyT1 tyT2
            unlessM (isSubtype p tyU tyU') (typeError p $ "doesn\'t match declared type : " <> show tyU <> " and " <> show tyU')
            return tyT

typeOf (TUnpack p tyX x t1 t2) = do
    tyT1 <- lcst p =<< typeOf t1
    case tyT1 of
         (TySome tyY tyBound tyT11) -> do
            withTmpStateT (addTypeVar tyX tyBound) $ do
                withTmpStateT (addVar x tyT11) $ do
                    typeShift p (-2) =<< typeOf t2
         _ -> typeError p "existential type expected"

isSubtype :: SourcePos -> Type -> Type -> Eval Bool
isSubtype p tyS tyT = do
    x <- typeEq p tyS tyT
    if x
    then return True
    else do
        tyS' <- simplifyType p tyS
        tyT' <- simplifyType p tyT
        case (tyS', tyT') of
              (_, TyTop) -> return True
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (isSubtype p tyT1 tyS1) <*> (isSubtype p tyS2 tyT2)

              (TyRecord f1, TyRecord f2) ->
                all (id) <$> (sequence (uncurry (isSubtype p) <$> (Map.elems $ Map.intersectionWith (,) f1 f2)))

              (TyVar _ _, _) -> do
                 x <- promote p tyS'
                 case x of
                      Just ty -> isSubtype p ty tyT
                      Nothing -> return False

              (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                    x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                    if x
                    then withTmpStateT (addTypeVar tyX1 tyT1) $ isSubtype p tyS2 tyT2
                    else return False

              (TyAbs tyX1 k1 tyS2, TyAbs _ k2 tyT2) | k1 == k2 -> do
                    withTmpStateT (addTypeVar tyX1 (makeTop k1)) $ isSubtype p tyS2 tyT2

              (TyApp _ _, _) -> do
                    x <- promote p tyS'
                    case x of
                        Just ty -> isSubtype p ty tyT
                        Nothing -> return False

              (TySome tyX1 tyS1 tyS2, TySome _ tyT1 tyT2) -> do
                    x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                    if x
                    then withTmpStateT (addTypeVar tyX1 tyT1) $ isSubtype p tyS2 tyT2
                    else return False

              _ -> return False

ok = return.return
notFound = return Nothing

joinTypes :: SourcePos ->  Type -> Type -> Eval Type
joinTypes p tyS tyT = do
    x <- isSubtype p tyS tyT
    if x
    then return tyT
    else do y <- isSubtype p tyT tyS
            if y
            then return tyS
            else do z <- (,) <$> simplifyType p tyS <*> simplifyType p tyT
                    case z of
                         (TyRecord fS, TyRecord fT) -> do
                            fTS <- traverse f (Map.toList $ Map.intersectionWith (,) fS fT)
                            return $ TyRecord (Map.fromList fTS)
                            where f (k, (tyS, tyT)) = ((,) k) <$> joinTypes p tyS tyT

                         (TyAll tyX tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                            x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                            if x
                            then withTmpStateT (addTypeVar tyX tyT1) $ TyAll tyX tyS1 <$> joinTypes p tyT1 tyT2
                            else return TyTop

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            ty1 <- meetTypes p tyS1 tyT1
                            case ty1 of
                                 Just x -> TyArrow x <$> joinTypes p tyS2 tyT2
                                 Nothing -> return TyTop

                         _ -> return TyTop

meetTypes :: SourcePos -> Type -> Type -> Eval (Maybe Type)
meetTypes p tyS tyT = do
    x <- isSubtype p tyS tyT
    if x
    then ok tyS
    else do y <- isSubtype p tyT tyS
            if y
            then ok tyT
            else do z <- (,) <$> simplifyType p tyS <*> simplifyType p tyT
                    case z of
                         (TyRecord fS, TyRecord fT) -> do
                            fST <- sequence $ fmap (uncurry f)
                                            $ Map.toList
                                            $ merge (mapMaybeMissing $ \k ty -> return (Just ty, Nothing))
                                                    (mapMaybeMissing $ \k ty -> return (Nothing, Just ty))
                                                    (zipWithMaybeMatched $ \k tySi tyTi -> return (Just tySi, Just tyTi))
                                                    fS fT
                            ok $ TyRecord $ Map.fromList $ catMaybes fST
                            where f k (Just tySi, Just tyTi) = fmap ((,) k) <$> meetTypes p tySi tyTi
                                  f k (Just tySi, Nothing) = ok (k, tySi)
                                  f k (Nothing, Just tyTi) = ok (k, tyTi)

                         (TyAll tyX tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                            x <- (&&) <$> (isSubtype p tyS1 tyT1) <*> (isSubtype p tyT1 tyS1)
                            if x
                            then notFound
                            else withTmpStateT (addTypeVar tyX tyT1) $ do
                                    y <- meetTypes p tyT1 tyT2
                                    case y of
                                         Just ty -> ok $ TyAll tyX tyS1 ty
                                         Nothing -> notFound

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            j <- (,) <$> joinTypes p tyS1 tyT1 <*> meetTypes p tyS2 tyT2
                            case j of
                                 (ty1, Just ty2) -> ok $ TyArrow ty1 ty2
                                 (ty1, Nothing) -> notFound

                         _ -> notFound

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
    where message = "Argument error, expected " <> show expected  <> ". Got " <> show actual <> "."

isStar :: SourcePos -> Type -> Eval Bool
isStar p ty = (==) Star <$> kindOf p ty

getKind :: SourcePos -> VarName -> Eval Kind
getKind p v = do
    x <- flip(getBinding p) v =<< get
    case x of
        (Just (TypeVarBind ty)) -> kindOf p ty
        (Just (TypeAddBind _ (Just k))) -> return k
        (Just (TypeAddBind _ Nothing)) -> lift $ throwE $ "No kind recorded for variable " <> show v <> " : " <> show x
        _ -> lift $ throwE $ "Wrong kind of binding for variable  " <> show v <> " : " <> show x

kindOf :: SourcePos -> Type -> Eval Kind
kindOf p (TyRecord fields) = do
    unlessM(all (id) <$> sequence (isStar p <$> (Map.elems fields))) (typeError p "Kind * expected")
    return Star

kindOf p (TyVar i _) = getKind p i

kindOf p (TyAll tyX tyT1 tyT2) = do
    withTmpStateT (addTypeVar tyX tyT1) $ do
        unlessM (isStar p tyT2) (typeError p "star kind expected")
        return Star

kindOf p (TyAbs tyX knK1 tyT2) = do
    withTmpStateT (addTypeVar tyX (makeTop knK1)) $ Arrow knK1 <$> kindOf p tyT2

kindOf p x@(TyApp tyT1 tyT2) = do
    knK1 <- kindOf p tyT1
    knK2 <- kindOf p tyT2
    names <- get
    case knK1 of
         (Arrow knK11 knK12) | knK2 == knK11 -> return knK12
         (Arrow knK11 knK12) -> typeError p "parameter kind mismatch"
         _ -> typeError p $ "arrow kind expected " <> show x <> " - " <> show knK1 <> show knK2 <> " : " <> show names

kindOf p (TySome tyX tyT1 tyT2) = do
    withTmpStateT (addTypeVar tyX tyT1) $ do
        unlessM (isStar p tyT2) (typeError p "star kind expected")
        return Star

kindOf p (TyArrow tyT1 tyT2) = do
    unlessM (isStar p tyT1) (typeError p "star kind expected")
    unlessM (isStar p tyT2) (typeError p "star kind expected")
    return Star

kindOf _ _ = return Star

typeEq :: SourcePos -> Type -> Type -> Eval Bool
typeEq p t1 t2 = do
    tyS <- simplifyType p t1
    tyT <- simplifyType p t2
    n <- get
    case (tyS, tyT) of
         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2
         (TyString, TyString) -> return True
         (TyTop, TyTop) -> return True
         (TyUnit, TyUnit) -> return True
         (TyID x, TyID y) -> return $ x == y
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

         (TyBool, TyBool) -> return True
         (TyNat, TyNat) -> return True

         (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
         (TyRecord f1, TyRecord f2) -> all (id) <$> sequence (uncurry (typeEq p) <$> Map.elems (Map.intersectionWith (,) f1 f2))

         (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> withTmpStateT (addName tyX1) $ do
             (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2

         (TySome tyX1 tyS1 tyS2, TySome _ tyT1 tyT2) -> withTmpStateT (addName tyX1) $ do
             (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2

         (TyAbs x k1 ty1, TyAbs _ k2 ty2) | k1 == k2 -> withTmpStateT (addName x) $ typeEq p ty1 ty2
         (TyApp tyS1 tyS2, TyApp tyT1 tyT2) -> (&&) <$> typeEq p tyS1 tyT1 <*> typeEq p tyS2 tyT2
         _ -> return False

computeType :: SourcePos -> Type -> Eval (Maybe Type)
computeType p (TyApp (TyAbs _ _ ty1) ty2) = Just <$> typeSubstitutionTop p ty2 ty1
computeType p (TyVar i _) = do
    n <- get
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

promote :: SourcePos -> Type -> Eval (Maybe Type)
promote p (TyVar i _) = do
    n <- get
    x <- getBinding p n i
    case x of
         Just (TypeVarBind ty) -> return $ Just ty
         _ -> return Nothing
promote p (TyApp tyS tyT) = fmap(flip(TyApp) tyT) <$> promote p tyS
promote _ _ = return Nothing

lcst :: SourcePos -> Type -> Eval Type
lcst p ty = do
    tyS <- simplifyType p ty
    x <- promote p tyS
    case x of
        Just ty -> lcst p ty
        Nothing -> return tyS
