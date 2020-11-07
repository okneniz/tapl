module Language.TAPL.FullRef.TypeChecker where

import qualified Data.Map.Lazy as Map
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.List (sort, intercalate, (\\))

import Control.Monad (when, foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullRef.Types
import Language.TAPL.FullRef.Pretty
import Language.TAPL.FullRef.Context

typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- getNames
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError p "var type error"

typeOf (TAbs _ x tyT1 t2) = do
    withTmpStateT (\s -> s { names = addVar x tyT1 (names s) }) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

typeOf (TApp p t1 t2) = do
    ty1 <- simplifyType =<< typeOf t1
    ty2 <- simplifyType =<< typeOf t2
    case ty1 of
         (TyArrow ty1' ty2') -> do
            unlessM (ty2 <: ty1') $ do
                ty1p <- renderType ty1
                ty2p <- renderType ty2
                typeError p $ "incorrect application " ++ ty2p ++ " to " ++ ty1p
            return ty2'
         TyBot -> return TyBot
         _ -> do
            ty1p <- renderType ty1
            typeError p $ "arrow type expected, insted" ++ ty1p

typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf p t1 t2 t3) = do
  ty1 <- typeOf t1
  unlessM (ty1 <: TyBool) (unexpectedType p TyBool ty1)
  ty2 <- typeOf t2
  ty3 <- typeOf t3
  joinTypes ty2 ty3

typeOf (TLet _ x t1 t2) = do
    ty1 <- typeOf t1
    withTmpStateT (\s -> s { names = addVar x ty1 (names s) }) $ do
        ty2 <- typeOf t2
        return $ typeShift (-1) ty2

typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = (,) k <$> typeOf v

typeOf (TProj p t k) = do
    ty <- simplifyType =<< typeOf t
    case (ty, k) of
         (TyRecord fs, _) ->
            case Map.lookup k fs of
                 Just x -> return x
                 _ -> typeError p $ "label " ++ show k ++ " not found"
         (TyRecord fs, _) -> typeError p "invalid keyword for record"
         (TyProduct x _, "0") -> return x
         (TyProduct _ x, "1") -> return x
         (TyProduct _ _, _) -> typeError p "invalid index for pair"
         _ -> typeError p "invalid projection"

typeOf (TCase p v branches) = do
    ty' <- simplifyType =<< typeOf v
    case ty' of
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
         x -> (typeError p $ "Invalid context for case statement " ++ show x)

typeOf (TFix p t1) = do
    tyT1 <- simplifyType =<< typeOf t1
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            unlessM (tyT12 <: tyT11) (typeError p  "result of body not compatible with domain")
            return tyT12
         TyBot -> return TyBot
         _ -> typeError p  "arrow type expected"

typeOf (TTag p key t tyT) = do
    tyT' <- simplifyType tyT
    case tyT' of
         TyVariant tys ->
            case Map.lookup key tys of
                 Just expected -> do
                    actual <- typeOf t
                    unlessM (actual <: expected) (unexpectedType p expected actual)
                    return tyT
                 _ -> typeError p $ "label " ++ key ++ " not found"
         _ -> typeError p $ "Annotation is not a variant type"

typeOf (TAscribe p t ty) = do
    ty' <- typeOf t
    unlessM (ty' <: ty) (typeError p "body of as-term does not have the expected type")
    return ty

typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit

typeOf (TRef _ t) = TyRef <$> typeOf t
typeOf (TLoc p t) = typeError p $ "locations are not supposed to occur in source programs!"

typeOf (TDeref p t) = do
    ty <- simplifyType =<< typeOf t
    case ty of
        TyRef x -> return x
        TyBot -> return TyBot
        TySource x -> return x
        x -> typeError p $ "argument of ! is not a Ref or Source"

typeOf (TAssign p t1 t2) = do
  ty1 <- simplifyType =<< typeOf t1
  ty2 <- simplifyType =<< typeOf t2
  case ty1 of
       (TyRef tyT1) -> do
            unlessM (ty2 <: tyT1) (typeError p $ "arguments of := are incompatible 1") -- TODO
            return TyUnit
       TyBot -> const TyBot <$> typeOf t2
       TySink tyT1 -> do
            unlessM (ty2 <: tyT1) (typeError p $ "arguments of := are incompatible 2") -- TODO
            return TyUnit
       _ -> typeError p $ "invalid assignment of " ++ show ty1 ++ " to " ++ show ty2

typeOf (TFloat _ _) = return TyFloat
typeOf (TTimesFloat p t1 t2) = do
    ty1 <- simplifyType =<< typeOf t1
    unlessM (typeEq ty1 TyFloat) (unexpectedType p TyFloat ty1)
    ty2 <- simplifyType =<< typeOf t2
    unlessM (typeEq ty2 TyFloat) (unexpectedType p TyFloat ty2)
    return TyFloat

typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
  ty <- simplifyType =<< typeOf t
  unlessM (typeEq ty TyNat)(unexpectedType p TyNat ty)
  return TyNat

typeOf (TPred p t) = do
  ty <- simplifyType =<< typeOf t
  unlessM (typeEq ty TyNat)(unexpectedType p TyNat ty)
  return TyNat

typeOf (TIsZero p t) = do
  ty <- simplifyType =<< typeOf t
  unlessM (typeEq ty TyNat)(unexpectedType p TyNat ty)
  return TyBool

typeOf (TPair _ t1 t2) = TyProduct <$> (typeOf t1) <*> (typeOf t2)

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

unexpectedType :: SourcePos -> Type -> Type -> Eval a
unexpectedType p expected actual = do
    tyE <- renderType expected
    tyA <- renderType actual
    typeError p $ "expected type " ++ tyE ++ ", actual " ++ tyA

(<:) :: Type -> Type -> Eval Bool
(<:) tyS tyT = do
    x <- typeEq tyS tyT
    if x
    then return True
    else do
        tyS' <- simplifyType tyS
        tyT' <- simplifyType tyT
        case (tyS', tyT) of
              (_, TyTop) -> return True
              (TyBot, _) -> return True
              (TyRef ty1, TyRef ty2) -> (&&) <$> (ty1 <: ty2) <*> (ty2 <: ty1)
              (TyRef ty1, TySource ty2) -> ty1 <: ty2
              (TySource ty1, TySource ty2) -> ty1 <: ty2
              (TyRef ty1, TySink ty2) -> ty2 <: ty1
              (TySink ty1, TySink ty2) -> ty2 <: ty1
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyS1 <: tyT1) <*> (tyS2 <: tyT2)
              (TyProduct tyS1 tyS2, TyProduct tyT1 tyT2) -> (&&) <$> (tyS1 <: tyT1) <*> (tyS2 <: tyT2)
              (TyRecord f1, TyRecord f2) ->
                all (id) <$> (sequence (uncurry (<:) <$> (Map.elems $ Map.intersectionWith (,) f1 f2)))
              (TyVariant f1, TyVariant f2) ->
                all (id) <$> (sequence (uncurry (<:) <$> (Map.elems $ Map.intersectionWith (,) f1 f2)))
              _ -> return False
    where subs f1 f2 = uncurry (<:) <$> fs f1 f2
          fs f1 f2 = Map.elems $ Map.intersectionWith (,) f1 f2

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    ty1' <- simplifyType ty1
    ty2' <- simplifyType ty2
    n <- getNames
    case (ty1', ty2') of
        (TyTop, TyTop) -> return True
        (TyBot, TyBot) -> return True
        (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
        (TyString, TyString) -> return True
        ((TyID x), (TyID y)) -> return $ x == y
        (TyFloat, TyFloat) -> return True
        (TyUnit, TyUnit) -> return True
        (TyRef t1, TyRef t2) -> typeEq t1 t2
        (TySource t1, TySource t2) -> typeEq t1 t2
        (TySink t1, TySink t2) -> typeEq t1 t2

        (TyVar _ i, _) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x ty2'
                _ -> return False

        (_, TyVar _ i) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq ty1' x
                _ -> return False

        (TyVar _ i, TyVar _ j) | i == j -> return True

        (TyBool, TyBool) -> return True
        (TyNat, TyNat) -> return True
        (TyProduct tyS1 tyS2, TyProduct tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2

        (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
        (TyRecord f1, TyRecord f2) ->
          all (id) <$> sequence (uncurry typeEq <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

        (TyVariant f1, TyVariant f2) | (Map.keys f1) /= (Map.keys f2) -> return False
        (TyVariant f1, TyVariant f2) ->
          all (id) <$> sequence (uncurry typeEq <$> (Map.elems $ Map.intersectionWith (,) f1 f2))

        _ -> return False

joinTypes :: Type -> Type -> Eval Type
joinTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyT
    else do y <- tyT <: tyS
            if y
            then return tyS
            else do tyS' <- simplifyType tyS
                    tyT' <- simplifyType tyT
                    case (tyS',tyT') of
                         (TyRecord fS, TyRecord fT) -> do
                            fTS <- sequence $ fmap f $ Map.toList $ Map.intersectionWith (,) fS fT
                            return $ TyRecord (Map.fromList fTS)
                            where f (k, (tyS, tyT)) = ((,) k) <$> joinTypes tyS tyT

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) ->
                            TyArrow <$> meetTypes tyS1 tyT1 <*> joinTypes tyS2 tyT2

                         (TyRef tyT1, TyRef tyT2) -> do -- (* Warning: this is incomplete... *)
                            -- https://github.com/enaudon/TAPL/blob/master/source/fullref/core.ml#L291
                            x <- (&&) <$> (tyT1 <: tyT2) <*> (tyT2 <: tyT1)
                            if x
                            then return $ TyRef tyT1
                            else TySource <$> joinTypes tyT1 tyT2
                         (TySource tyT1, TySource tyT2) -> TySource <$> joinTypes tyT1 tyT2
                         (TyRef tyT1, TySource tyT2) -> TySource <$> joinTypes tyT1 tyT2
                         (TySource tyT1, TyRef tyT2) -> TySource <$> joinTypes tyT1 tyT2
                         (TySink tyT1, TySink tyT2) -> TySource <$> meetTypes tyT1 tyT2
                         (TyRef tyT1, TySink tyT2) -> TySource <$> meetTypes tyT1 tyT2
                         (TySink tyT1, TyRef tyT2) -> TySource <$> meetTypes tyT1 tyT2
                         _ -> return TyTop

meetTypes :: Type -> Type -> Eval Type
meetTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyS
    else do y <- tyT <: tyS
            if y
            then return tyT
            else do tyS' <- simplifyType tyS
                    tyT' <- simplifyType tyT
                    case (tyS',tyT') of
                         (TyRecord fS, TyRecord fT) -> do
                            fST <- sequence $ fmap (uncurry f)
                                            $ Map.toList
                                            $ merge (mapMaybeMissing $ \k ty -> return (Just ty, Nothing))
                                                    (mapMaybeMissing $ \k ty -> return (Nothing, Just ty))
                                                    (zipWithMaybeMatched $ \k tySi tyTi -> return (Just tySi, Just tyTi))
                                                    fS fT
                            return $ TyRecord (Map.fromList fST)
                            where f k (Just tySi, Just tyTi) = (,) k <$> meetTypes tySi tyTi
                                  f k (Just tySi, Nothing) = return $ (k, tySi)
                                  f k (Nothing, Just tyTi) = return $ (k, tyTi)

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) ->
                            TyArrow <$> joinTypes tyS1 tyT1 <*> meetTypes tyS2 tyT2

                         (TyRef tyT1, TyRef tyT2) -> do -- (* Warning: this is incomplete... *)
                            -- https://github.com/enaudon/TAPL/blob/master/source/fullref/core.ml#L339
                            x <- (&&) <$> (tyT1 <: tyT2) <*> (tyT2 <: tyT1)
                            if x
                            then return $ TyRef tyT1
                            else TySource <$> meetTypes tyT1 tyT2

                         (TySource tyT1, TySource tyT2) -> TySource <$> meetTypes tyT1 tyT2
                         (TyRef tyT1, TySource tyT2) -> TySource <$> meetTypes tyT1 tyT2
                         (TySource tyT1, TyRef tyT2) -> TySource <$> meetTypes tyT1 tyT2
                         (TySink tyT1, TySink tyT2) -> TySource <$> joinTypes tyT1 tyT2
                         (TyRef tyT1, TySink tyT2) -> TySource <$> joinTypes tyT1 tyT2
                         (TySink tyT1, TyRef tyT2) -> TySource <$> joinTypes tyT1 tyT2
                         _ -> return TyBot

computeType :: Type -> Eval (Maybe Type)
computeType (TyVar i _) = do
    n <- getNames
    if isTypeAbb n i
    then return $ getTypeAbb n i
    else return Nothing
computeType _ = return Nothing

simplifyType :: Type -> Eval Type
simplifyType ty = do
    n <- computeType ty
    case n of
         Just x -> simplifyType x
         _ -> return ty
