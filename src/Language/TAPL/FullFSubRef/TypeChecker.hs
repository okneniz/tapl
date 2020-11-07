module Language.TAPL.FullFSubRef.TypeChecker (typeOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate, sort)
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Control.Monad (when, unless, liftM, liftM2, when, unless, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullFSubRef.Types
import Language.TAPL.FullFSubRef.Context

typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- getNames
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError p "var type error"

typeOf (TAbs _ x tyT1 t2) = do
    withTmpStateT (\s -> s { names = addVar x tyT1 (names s)}) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

typeOf r@(TApp p t1 t2) = do
    tyT1 <- lcst =<< typeOf t1
    tyT2 <- typeOf t2
    n <- get
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            s <- simplifyType tyT11
            unlessM (tyT2 <: tyT11) (typeError p $ "parameter type missmatch " ++ show tyT11 ++ " : " ++ show tyT2)
            return tyT12
         TyBot -> return TyBot
         x -> typeError p $ "arrow type expected " ++ show x ++ " : " ++ show n

typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: TyBool) (typeError p $ "guard of condition have not a Bool type (" ++ show ty1 ++ ")")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    joinTypes ty2 ty3

typeOf (TLet _ x t1 t2) = do
    ty1 <- typeOf t1
    withTmpStateT (\s -> s { names = addVar x ty1 (names s)}) $ do
        ty2 <- typeOf t2
        return $ typeShift (-1) ty2

typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = ((,) k) <$> typeOf v

typeOf s@(TProj p t key) = do
    ty <- lcst =<< typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         TyBot -> return TyBot
         x -> typeError p $ "Expected record type: " ++ show s

typeOf (TProj p _ _) = typeError p "invalid lookup operation"

typeOf (TCase p v branches) = do
    ty1 <- lcst =<< typeOf v
    case ty1 of
         TyVariant fields -> do
            when (not $ null invalidCaseBranches)
                 (typeError p $ "Invalid case branches : " ++ intercalate ", " invalidCaseBranches)

            when (not $ null absentCaseBranches)
                 (typeError p $ "Absent case branches : " ++ intercalate ", " absentCaseBranches)

            cases <- sequence $ fmap caseType $ Map.toList $ Map.intersectionWith (,) branches fields
            foldM joinTypes TyBot cases
            where variantKeys = Map.keys fields
                  branchesKeys = Map.keys branches
                  invalidCaseBranches = branchesKeys \\ variantKeys
                  absentCaseBranches = variantKeys \\ branchesKeys
                  caseType (caseName, ((varName, t), vty)) =
                    withTmpStateT id $ do
                        modifyNames $ addVar varName vty
                        typeShift (-1) <$> typeOf t

         TyBot -> return TyBot
         _ -> typeError p $ "Expected variant type"

typeOf (TFix p t1) = do
    tyT1 <- lcst =<< typeOf t1
    case tyT1 of
        (TyArrow tyT11 tyT12) -> do
            unlessM (tyT12 <: tyT11) (typeError p $ "result of body not compatible with domain " ++ show tyT11 ++ " and " ++ show tyT12)
            return tyT12
        TyBot -> return TyBot
        _ -> typeError p  "arrow type expected"

typeOf (TTag p li t tyT) = do
   ty <- simplifyType tyT
   case ty of
        (TyVariant fields) ->
            case Map.lookup li fields of
                 Just tyTiExpected -> do
                    tyTi <- typeOf t
                    unlessM (tyTi <: tyTiExpected) (typeError p "field does not have expected type")
                    return tyT
                 _ -> typeError p "field does not have expected type"
        TyBot -> return TyBot
        _ -> typeError p "Expected record type"

typeOf (TAscribe p t1 tyT) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: tyT) (typeError p "body of as-term does not have the expected type")
    return tyT

typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit
typeOf (TRef _ t) = TyRef <$> typeOf t
typeOf (TLoc p _) = typeError p "locations are not supposed to occur in source programs!"

typeOf (TDeref p t1) = do
    ty1 <- lcst =<< typeOf t1
    case ty1 of
         TyRef tyT1 -> return tyT1
         TyBot -> return TyBot
         TySource tyT1 -> return tyT1
         _ -> typeError p "argument of ! is not a Ref or Source"

typeOf (TAssign p t1 t2) = do
    ty1 <- lcst =<< typeOf t1
    ty2 <- typeOf t2
    case ty1 of
         (TyRef tyT1) -> do
            unlessM (ty2 <: tyT1) (typeError p $ "arguments of := are incompatible " ++ show ty2 ++ " -> " ++ show tyT1)
            return TyUnit
         TyBot -> typeOf t2 >> return TyBot
         (TySink tyT1) -> do
            unlessM (ty2 <: tyT1) (typeError p $ "arguments of := are incompatible " ++ show ty2 ++ " - " ++ show tyT1)
            return TyUnit
         _ -> typeError p "first argument of := is not a Ref or Sink"

typeOf (TError _) = return TyBot
typeOf (TFloat _ _) = return TyFloat

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: TyFloat) (argumentError p TyFloat ty1)
    ty2 <- typeOf t2
    unlessM (ty2 <: TyFloat) (argumentError p TyFloat ty2)
    return TyFloat

typeOf (TTAbs p tyX tyT1 t2) = withTmpStateT (\s -> s { names = addTypeVar tyX tyT1 (names s)}) $ do
    TyAll tyX tyT1 <$> typeOf t2

typeOf (TTApp p t1 tyT2) = do
    tyT1 <- lcst =<< typeOf t1
    case tyT1 of
        TyAll _ tyT11 tyT12 -> do
            unlessM (tyT2 <: tyT11) (typeError p "type parameter type mismatch")
            return $ typeSubstitutionTop tyT2 tyT12
        _ -> typeError p "universal type expected"

typeOf (TTry _ t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    joinTypes ty1 ty2

typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
    ty <- typeOf t
    unlessM (ty <: TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TPred p t) = do
    ty <- typeOf t
    unlessM (ty <: TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TIsZero p t) = do
    ty <- typeOf t
    unlessM (ty <: TyNat) (argumentError p TyNat ty)
    return TyBool

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
    where message = "Argument error, expected " ++ show expected  ++ ". Got " ++ show actual ++ "."

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    tyS <- simplifyType ty1
    tyT <- simplifyType ty2
    n <- getNames
    case (tyS, tyT) of
      (TyTop, TyTop) -> return True
      (TyBot, TyBot) -> return True
      (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
      (TyString, TyString) -> return True
      (TyID x, TyID y) -> return $ x == y
      (TyFloat, TyFloat) -> return True
      (TyUnit, TyUnit) -> return True
      (TyRef ty1, TyRef ty2) -> typeEq ty1 ty1
      (TySource ty1, TySource ty2) -> typeEq ty1 ty1
      (TySink ty1, TySink ty2) -> typeEq ty1 ty1

      (TyVar i _, _) | isTypeAdd n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x tyT
                _ -> return False

      (_, TyVar i _) | isTypeAdd n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x tyS
                _ -> return False

      (TyVar i _, TyVar j _) | i == j -> return True

      (TyBool, TyBool) -> return True
      (TyNat, TyNat) -> return True

      (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
      (TyRecord f1, TyRecord f2) -> all (id) <$> sequence (uncurry typeEq <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

      (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> withTmpStateT (\s -> s { names = addName tyX1 (names s) }) $ do
        (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2

      (TyVariant f1, TyVariant f2) | (Map.keys f1) /= (Map.keys f2) -> return False
      (TyVariant f1, TyVariant f2) ->
          all (id) <$> sequence (uncurry typeEq <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

      (TyInt, TyInt) -> return True
      _ -> return False

(<:) :: Type -> Type -> Eval Bool
(<:) tyS tyT = do
    x <- typeEq tyS tyT
    if x
    then return True
    else do
        tyS' <- simplifyType tyS
        tyT' <- simplifyType tyT
        case (tyS', tyT') of
              (_, TyTop) -> return True
              (TyBot, _) -> return True

              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyT1 <: tyS1) <*> (tyS2 <: tyT2)

              (TyRecord f1, TyRecord f2) ->
                all (id) <$> (sequence (uncurry (<:) <$> (Map.elems $ Map.intersectionWith (,) f1 f2)))

              (TyVariant f1, TyVariant f2) ->
                all (id) <$> (sequence (uncurry (<:) <$> (Map.elems $ Map.intersectionWith (,) f1 f2)))

              (TyVar _ _, _) -> do
                x <- promote tyS'
                case x of
                  Just ty -> ty <: tyT
                  Nothing -> return False

              (TyAll tyX1 tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                x <- (&&) <$> (tyS1 <: tyT1) <*> (tyT1 <: tyS1)
                if x
                then withTmpStateT (\s -> s { names = addTypeVar tyX1 tyT1 (names s)}) $ do { tyS2 <: tyT2 }
                else return False

              (TyRef ty1, TyRef ty2) -> (&&) <$> (ty1 <: ty2) <*> (ty2 <: ty1)
              (TyRef ty1, TySource ty2) -> ty1 <: ty2
              (TySource ty1, TySource ty2) -> ty1 <: ty2
              (TyRef ty1, TySink ty2) -> ty2 <: ty1
              (TySink ty1, TySink ty2) -> ty2 <: ty1

              _ -> return False

reasonable  x = (return.return) x
nvm = return Nothing

joinTypes :: Type -> Type -> Eval Type
joinTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyT
    else do y <- tyT <: tyS
            if y
            then return tyS
            else do z <- (,) <$> simplifyType tyS <*> simplifyType tyT
                    case z of
                         (TyRecord fS, TyRecord fT) -> do
                            fTS <- sequence $ fmap f $ Map.toList $ Map.intersectionWith (,) fS fT
                            return $ TyRecord (Map.fromList fTS)
                            where f (k, (tyS, tyT)) = ((,) k) <$> joinTypes tyS tyT

                         (TyAll tyX tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                            x <- (&&) <$> (tyS1 <: tyT1) <*> (tyT1 <: tyS1)
                            if x
                            then withTmpStateT (\s -> s { names = addTypeVar tyX tyT1 (names s)}) $ do
                                TyAll tyX tyS1 <$> joinTypes tyT1 tyT2
                            else return TyTop

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            ty1 <- meetTypes tyS1 tyT1
                            case ty1 of
                                 Just x -> TyArrow x <$> joinTypes tyS2 tyT2
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         (TyRef tyT1, TyRef tyT2) -> do
                            x <- (&&) <$> (tyT1 <: tyT2) <*> (tyT2 <: tyT1)
                            if x
                            then return $ TyRef tyT1
                            else TySource <$> joinTypes tyT1 tyT2

                         (TySource ty1, TySource ty2) -> TySource <$> joinTypes ty1 ty2
                         (TyRef ty1, TySource ty2) -> TySource <$> joinTypes ty1 ty2
                         (TySource ty1, TyRef ty2) -> TySource <$> joinTypes ty1 ty2

                         (TySink ty1, TySink ty2) -> do
                            x <- meetTypes ty1 ty2
                            case x of
                                 Just ty -> return $ TySink ty
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         (TyRef ty1, TySink ty2) -> do
                            x <- meetTypes ty1 ty2
                            case x of
                                 Just ty -> return $ TySink ty
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         (TySink ty1, TyRef ty2) -> do
                            x <- meetTypes ty1 ty2
                            case x of
                                 Just ty -> return $ TySink ty
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         _ -> return TyTop

meetTypes :: Type -> Type -> Eval (Maybe Type)
meetTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then reasonable tyS
    else do y <- tyT <: tyS
            if y
            then reasonable tyT
            else do z <- (,) <$> simplifyType tyS <*> simplifyType tyT
                    case z of
                         (TyRecord fS, TyRecord fT) -> do
                            fST <- sequence $ fmap (uncurry f)
                                            $ Map.toList
                                            $ merge (mapMaybeMissing $ \k ty -> return (Just ty, Nothing))
                                                    (mapMaybeMissing $ \k ty -> return (Nothing, Just ty))
                                                    (zipWithMaybeMatched $ \k tySi tyTi -> return (Just tySi, Just tyTi))
                                                    fS fT
                             -- TODO : WTF ? https://github.com/enaudon/TAPL/blob/master/source/fullsub/core.ml#L240
                             -- what is Nothing (not found in original implementation) in this context?
                             -- how to handle it?
                            reasonable $ TyRecord $ Map.fromList $ catMaybes fST
                            where f k (Just tySi, Just tyTi) = liftM ((,) k) <$> meetTypes tySi tyTi
                                  f k (Just tySi, Nothing) = reasonable (k, tySi)
                                  f k (Nothing, Just tyTi) = reasonable (k, tyTi)

                         (TyAll tyX tyS1 tyS2, TyAll _ tyT1 tyT2) -> do
                            x <- (&&) <$> (tyS1 <: tyT1) <*> (tyT1 <: tyS1)
                            if x
                            then nvm
                            else withTmpStateT (\s -> s { names = addTypeVar tyX tyT1 (names s)}) $ do
                                    y <- meetTypes tyT1 tyT2
                                    case y of
                                         Just ty -> reasonable $ TyAll tyX tyS1 ty
                                         Nothing -> nvm

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            j <- (,) <$> joinTypes tyS1 tyT1 <*> meetTypes tyS2 tyT2
                            case j of
                                 (ty1, Just ty2) -> reasonable $ TyArrow ty1 ty2
                                 (ty1, Nothing) -> nvm

                         (TyRef tyT1, TyRef tyT2) -> do
                            x <- (&&) <$> (tyT1 <: tyT2) <*> (tyT2 <: tyT1)
                            if x
                            then reasonable $ TyRef tyT1
                            else do
                                x <- meetTypes tyT1 tyT2
                                case x of
                                     Just x -> reasonable $ TySource x
                                     Nothing -> lift $ throwE "i don't know how to handle it"

                         (TySource tyT1, TySource tyT2) -> do
                            x <- meetTypes tyT1 tyT2
                            case x of
                                 Just ty -> reasonable $ TySource ty
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         (TyRef tyT1, TySource tyT2) -> do
                            x <- meetTypes tyT1 tyT2
                            case x of
                                 Just ty -> reasonable $ TySource ty
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         (TySource tyT1, TyRef tyT2) -> do
                            x <- meetTypes tyT1 tyT2
                            case x of
                                 Just ty -> reasonable $ TySource ty
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         (TySink tyT1, TySink tyT2) -> do
                            x <- meetTypes tyT1 tyT2
                            case x of
                                 Just ty -> reasonable $ TySource ty
                                 Nothing -> lift $ throwE "i don't know how to handle it"

                         (TyRef tyT1, TySink tyT2) -> do
                            ty <- joinTypes tyT1 tyT2
                            reasonable $ TySink ty

                         (TySink tyT1, TyRef tyT2) -> do
                            ty <- joinTypes tyT1 tyT2
                            reasonable $ TySink ty

                         _ -> reasonable TyBot

computeType :: Type -> Eval (Maybe Type)
computeType (TyVar i _) = do
    n <- getNames
    if isTypeAdd n i
    then return $ getTypeAbb n i
    else return Nothing

computeType _ = return Nothing

simplifyType :: Type -> Eval Type
simplifyType ty = do
    n <- computeType ty
    case n of
         Just x -> simplifyType x
         _ -> return ty

promote :: Type -> Eval (Maybe Type)
promote (TyVar i _) = do
    n <- getNames
    case getBinding n i of
         Just (TypeVarBind ty) -> return $ Just ty
         _ -> return Nothing
promote _ = return Nothing

lcst :: Type -> Eval Type
lcst ty = do
    tyS <- simplifyType ty
    x <- promote tyS
    case x of
        Just ty -> lcst ty
        Nothing -> return tyS
