module Language.TAPL.FullSub.TypeChecker (typeOf) where

import Data.List (tails, intercalate, all, (\\), sort)
import qualified Data.Map.Lazy as Map
import Data.Map.Merge.Strict (merge, mapMaybeMissing, zipWithMaybeMatched)
import Data.Maybe (catMaybes)

import Text.Parsec (SourcePos)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT, ok, nvm)
import Language.TAPL.FullSub.Types
import Language.TAPL.FullSub.Pretty
import Language.TAPL.FullSub.Context

data TypeError = TypeMissmatch SourcePos String

typeOf :: Term -> Eval Type
typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" <> show x <> " " <> show n <> " " <> show v <> ")"
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
            unlessM (ty2 <: ty1') $ do
                ty1p <- prettifyType ty1
                ty2p <- prettifyType ty2
                typeError p $ "incorrect application " <> show ty2p <> " to " <> show ty1p
            return ty2'
         _ -> do
            ty1p <- prettifyType ty1
            typeError p $ "arrow type expected, insted" <> show ty1p

typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: TyBool)
            (typeError p $ "guard of condition have not a " <> show TyBool <>  " type (" <> show ty1 <> ")")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    joinTypes ty2 ty3

typeOf (TRecord _ fields) = do
    tys <- mapM tyField (Map.toList fields)
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = (,) <$> return k <*> typeOf v

typeOf (TProj p t key) = do
    ty <- simplifyType =<< typeOf t
    case ty of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "label " <> show key <> " not found"
         _ -> typeError p "expected record type"

typeOf (TProj p _ _) = typeError p "invalid projection"

typeOf (TLet _ x t1 t2) = do
    ty1 <- typeOf t1
    withTmpStateT (addVar x ty1) $ do
        ty2 <- typeOf t2
        return $ typeShift (-1) ty2

typeOf (TFix p t1) = do
    tyT1 <- simplifyType =<< typeOf t1
    case tyT1 of
         (TyArrow tyT11 tyT12) -> do
            unlessM (tyT12 <: tyT11)
                    (typeError p  "result of body not compatible with domain")
            return tyT12
         _ -> typeError p  "arrow type expected"

typeOf (TString _ _) = return TyString
typeOf (TUnit _) = return TyUnit

typeOf (TAscribe p t ty) = do
    ty' <- typeOf t
    unlessM (ty' <: ty) (typeError p "body of as-term does not have the expected type")
    return ty

typeOf (TFloat _ _) = return TyFloat

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: TyFloat) (unexpectedType p TyFloat ty1)
    ty2 <- typeOf t2
    unlessM (ty2 <: TyFloat) (unexpectedType p TyFloat ty2)
    return TyFloat

typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
  ty <- typeOf t
  unlessM (ty <: TyNat) (unexpectedType p TyNat ty)
  return TyNat

typeOf (TPred p t) = do
  ty <- typeOf t
  unlessM (ty <: TyNat) (unexpectedType p TyNat ty)
  return TyNat

typeOf (TIsZero p t) = do
  ty <- typeOf t
  unlessM (ty <: TyNat) (unexpectedType p TyNat ty)
  return TyBool

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p <> ":" <> message

unexpectedType :: SourcePos -> Type -> Type -> Eval a
unexpectedType p expected actual = do
    tyE <- prettifyType expected
    tyA <- prettifyType actual
    typeError p $ "expected type " <> show tyE <> ", actual " <> show tyA

instance Show TypeError where
    show (TypeMissmatch p message) = show p <> ":" <> message

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    ty1' <- simplifyType ty1
    ty2' <- simplifyType ty2
    n <- get
    case (ty1', ty2') of
      (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1 <*> typeEq tyS2 tyT2
      (TyString, TyString) -> return True
      (TyTop, TyTop) -> return True
      (TyUnit, TyUnit) -> return True
      ((TyID x), (TyID y)) -> return $ x == y
      (TyFloat, TyFloat) -> return True

      (TyVar i _, _) | isTypeAbb n i -> do
            case getTypeAbb n i of
                Just x -> typeEq x ty2'
                _ -> return False

      (_, TyVar i _) | isTypeAbb n i -> do
            case getTypeAbb n i of
                Just x -> typeEq x ty1'
                _ -> return False

      (TyVar i _, TyVar j _) | i == j -> return True

      (TyBool, TyBool) -> return True
      (TyNat, TyNat) -> return True

      (TyRecord f1, TyRecord f2) | (sort $ Map.keys f1) /= (sort $ Map.keys f2) -> return False
      (TyRecord f1, TyRecord f2) ->
        all (id) <$> sequence (uncurry typeEq <$> Map.elems (Map.intersectionWith (,) f1 f2))

      _ -> return False

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
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyS1 <: tyT1) <*> (tyS2 <: tyT2)
              (TyRecord f1, TyRecord f2) ->
                all (id) <$> (sequence (uncurry (<:) <$> Map.elems (Map.intersectionWith (,) f1 f2)))
              _ -> return False
    where subs f1 f2 = uncurry (<:) <$> fs f1 f2
          fs f1 f2 = Map.elems $ Map.intersectionWith (,) f1 f2

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
                            fTS <- traverse f (Map.toList $ Map.intersectionWith (,) fS fT)
                            return $ TyRecord (Map.fromList fTS)
                            where f (k, (tyS, tyT)) = ((,) k) <$> joinTypes tyS tyT

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            j <- meetTypes tyS1 tyT1
                            case j of
                                 Just ty -> TyArrow ty <$> joinTypes tyS2 tyT2
                                 Nothing -> return TyTop

                         _ -> return TyTop

meetTypes :: Type -> Type -> Eval (Maybe Type)
meetTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then ok tyS
    else do y <- tyT <: tyS
            if y
            then ok tyT
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
                            ok $ TyRecord $ Map.fromList $ catMaybes fST
                            where f k (Just tySi, Just tyTi) = fmap ((,) k) <$> meetTypes tySi tyTi
                                  f k (Just tySi, Nothing) = ok (k, tySi)
                                  f k (Nothing, Just tyTi) = ok (k, tyTi)

                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> do
                            j <- (,) <$> joinTypes tyS1 tyT1 <*> meetTypes tyS2 tyT2
                            case j of
                                 (ty1, Just ty2) -> ok $ TyArrow ty1 ty2
                                 (ty1, Nothing) -> nvm

                         _ -> nvm
