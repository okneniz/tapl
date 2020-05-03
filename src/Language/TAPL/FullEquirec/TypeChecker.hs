module Language.TAPL.FullEquirec.TypeChecker (typeOf) where

import qualified Data.Map.Strict as Map
import Data.List (tails, (\\), intercalate)

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.FullEquirec.Types
import Language.TAPL.FullEquirec.Context

typeOf :: Term -> Eval Type
typeOf = infer

infer :: Term -> Eval Type
infer (TString _ _) = return TyString
infer (TFloat _ _) = return TyFloat
infer (TTrue _) = return TyBool
infer (TFalse _) = return TyBool

infer (TVar info v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError info "var type error"

infer (TAscribe info t1 ty) = do
    ty1 <- infer t1
    unlessM (typeEq ty ty1) (typeError info "body of as-term does not have the expected type")
    return ty

infer (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = do
            tyf <- infer v
            return (k, tyf)

infer (TLookup _ t (TInt info i)) = do
    ty <- infer t
    ty' <- simplifyType ty
    case (ty', i) of
         ((TyProduct x _), 0) -> return x
         ((TyProduct _ x), 1) -> return x
         ((TyProduct _ _), _) -> typeError info "invalid index for pair"
         (_, _)               -> typeError info "invalid lookup operation"

infer (TLookup _ t (TKeyword info key)) = do
    ty <- infer t
    ty' <- simplifyType ty
    case ty' of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError info $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         _ -> typeError info "invalid lookup operation"

infer (TLookup info _ _) = typeError info "invalid lookup operation"

infer (TAbs _ x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- infer t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

infer (TApp info t1 t2) = do
    tyT1 <- infer t1
    tyT2 <- infer t2
    tyT1' <- simplifyType tyT1
    case tyT1' of
         (TyArrow tyT11 tyT12) -> do
            x <- typeEq tyT2 tyT11
            if x
            then return tyT12
            else typeError info $ "incorrect application of abstraction " ++ show tyT2 ++ " to " ++ show tyT11
         _ -> typeError info $ "incorrect application " ++ show tyT1 ++ " and " ++ show tyT2

infer (TIf info t1 t2 t3) = do
    ty1 <- infer t1
    unlessM (typeEq ty1 TyBool)
            (typeError info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty1 ++ ")")
    ty2 <- infer t2
    ty3 <- infer t3
    unlessM (typeEq ty2 ty3)
           (typeError info $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")")
    return ty2

infer (TZero _) = return TyNat

infer (TSucc info t) = do
    ty <- infer t
    unlessM (typeEq ty TyNat) (argumentError info TyNat ty)
    return TyNat

infer (TPred info t) = do
    ty <- infer t
    unlessM (typeEq ty TyNat) (argumentError info TyNat ty)
    return TyNat

infer (TIsZero info t) = do
    ty <- infer t
    unlessM (typeEq ty TyNat) (argumentError info TyNat ty)
    return TyBool

infer (TTimesFloat info t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    unlessM (typeEq ty1 TyFloat) (argumentError info TyFloat ty1)
    unlessM (typeEq ty2 TyFloat) (argumentError info TyFloat ty2)
    return TyFloat

infer (TCase info v branches) = do
    v' <- infer v
    ty' <- simplifyType v'
    case ty' of
         TyVariant fields -> do
            when (not $ null invalidCaseBranches)
                 (typeError info $ "Invalid case branches : " ++ intercalate ", " invalidCaseBranches)

            when (not $ null absentCaseBranches)
                 (typeError info $ "Absent case branches : " ++ intercalate ", " absentCaseBranches)

            cases <- sequence $ fmap caseType $ Map.toList $ Map.intersectionWith (,) branches fields
            theSameTypes <- sequence $ [typeEq t1 t2 | (t1:ys) <- tails $ snd <$> cases, t2 <- ys]

            unless (all id theSameTypes)
                   (typeError info $ "Case branches have different types")

            return $ snd $ head cases

            where variantKeys = Map.keys fields
                  branchesKeys = Map.keys branches
                  invalidCaseBranches = branchesKeys \\ variantKeys
                  absentCaseBranches = variantKeys \\ branchesKeys
                  caseType (caseName, ((varName, t), vty)) =
                    withTmpStateT (addVar varName vty) $ do
                        ty <- infer t
                        return (caseName, ty)

         x -> (typeError info $ "Invalid context for case statement " ++ show x)

infer (TTag info key t1 ty) = do
    ty1 <- infer t1
    ty' <- simplifyType ty
    case ty' of
         TyVariant tys ->
            case Map.lookup key tys of
                 Just x -> do
                    whenM (typeEq x ty') (typeError info "field does not have expected type")
                    return ty
                 _ -> typeError info $ "label " ++ key ++ " not found"
         _ -> typeError info $ "Annotation is not a variant type : " ++ show ty1

infer (TLet _ x t1 t2) = do
    ty1 <- infer t1
    withTmpStateT (addVar x ty1) $ do
        ty2 <- infer t2
        return $ typeShift (-1) ty2

infer (TUnit _) = return TyUnit

infer (TPair _ t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    return $ TyProduct ty1 ty2

infer (TFix info t1) = do
    tyT1 <- infer t1
    tyT1' <- simplifyType tyT1
    case tyT1' of
        (TyArrow tyT11 tyT12) -> do
            unlessM (typeEq tyT11 tyT12) (typeError info $ "result of body not compatible with domain " ++ show tyT11 ++ " and " ++ show tyT12)
            return tyT12
        _ -> typeError info  "arrow type expected"

whenM :: Monad m => m Bool -> m () -> m ()
whenM p s = do
    x <- p
    when x s

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = do
    x <- p
    unless x s

typeError :: Info -> String -> Eval a
typeError info message = lift $ throwE $ show info ++ ":" ++ message

argumentError :: Info -> Type -> Type -> Eval a
argumentError info expected actual = typeError info message
    where message = "Argument error, expected " ++ show expected  ++ ". Got " ++ show actual ++ "."

typeEq :: Type -> Type -> Eval Bool
typeEq tyS tyT = do
    names <- get
    return $ typeEq' [] names tyS tyT
    where mem _ [] = False
          mem y (x:xs) = (y == x) || (mem y xs)
          typeEq' seen ns ty1 ty2 =
            if mem (tyS,tyT) seen
            then True
            else typeEq'' [] ns ty1 ty2
          typeEq'' _ _ TyString TyString = True
          typeEq'' _ _ TyFloat TyFloat = True
          typeEq'' _ _ TyUnit TyUnit = True
          typeEq'' _ _ TyBool TyBool = True
          typeEq'' _ _ TyNat TyNat = True
          typeEq'' seen n (TyRec _ tyS1) _ = typeEq' ((tyS, tyT):seen) n (typeSubstitutionTop tyS tyS1) tyT
          typeEq'' seen n _ (TyRec _ tyT1) = typeEq' ((tyS, tyT):seen) n tyS (typeSubstitutionTop tyT tyT1)
          typeEq'' _ _ (TyID b1) (TyID b2) = b1 == b2

          typeEq'' seen n (TyVar i _) tyT1 | isTypeAbb n i =
            case (getTypeAbb n i) of
                Just x -> typeEq' seen n x tyT1
                _ -> False

          typeEq'' seen n tyS1 (TyVar i _) | isTypeAbb n i =
            case (getTypeAbb n i) of
                Just x -> typeEq' seen n tyS1 x
                _ -> False

          typeEq'' _ _ (TyVar _ i) (TyVar _ j) = i == j
          typeEq'' seen n (TyArrow tyS1 tyS2) (TyArrow tyT1 tyT2) = (typeEq' seen n tyS1 tyT1) && (typeEq' seen n tyS2 tyT2)

          typeEq'' _ _ (TyRecord fields1) (TyRecord fields2) | (Map.size fields1) /= (Map.size fields2) = False
          typeEq'' seen n (TyRecord fields1) (TyRecord fields2) =
            all (\(ty1,ty2) -> typeEq' seen n ty1 ty2)
              $ Map.elems
              $ Map.intersectionWith (,) fields1 fields2

          typeEq'' _ _ (TyVariant fields1) (TyVariant fields2) | (Map.size fields1) /= (Map.size fields2) = False
          typeEq'' seen n (TyVariant fields1) (TyVariant fields2) =
            all (\(ty1,ty2) -> typeEq' seen n ty1 ty2)
              $ Map.elems
              $ Map.intersectionWith (,) fields1 fields2

          typeEq'' seen n (TyProduct t1 t2) (TyProduct t11 t12) = (typeEq' seen n t1 t11) && (typeEq' seen n t2 t12)
          typeEq'' _ _ _ _ = False

computeType :: Type -> Eval (Maybe Type)
computeType ty@(TyRec _ tyS) = do
    return $ Just $ typeSubstitutionTop ty tyS

computeType (TyVar i _) = do
    n <- get
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
