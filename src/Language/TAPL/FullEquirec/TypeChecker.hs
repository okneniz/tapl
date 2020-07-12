module Language.TAPL.FullEquirec.TypeChecker (typeOf) where

import qualified Data.Map.Lazy as Map
import Data.List (tails, (\\), intercalate)

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.FullEquirec.Types
import Language.TAPL.FullEquirec.Context

typeOf :: Term -> Eval Type
typeOf (TString _ _) = return TyString
typeOf (TFloat _ _) = return TyFloat
typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError p "var type error"

typeOf (TAscribe p t1 ty) = do
    ty1 <- typeOf t1
    unlessM (typeEq ty ty1) (typeError p "body of as-term does not have the expected type")
    return ty

typeOf (TRecord _ fields) = do
    tys <- sequence $ fmap tyField $ Map.toList fields
    return $ TyRecord $ Map.fromList tys
    where tyField (k,v) = do
            tyf <- typeOf v
            return (k, tyf)

typeOf (TLookup _ t (TInt p i)) = do
    ty <- typeOf t
    ty' <- simplifyType ty
    case (ty', i) of
         ((TyProduct x _), 0) -> return x
         ((TyProduct _ x), 1) -> return x
         ((TyProduct _ _), _) -> typeError p "invalid index for pair"
         (_, _)               -> typeError p "invalid lookup operation"

typeOf (TLookup _ t (TKeyword p key)) = do
    ty <- typeOf t
    ty' <- simplifyType ty
    case ty' of
         (TyRecord fields) ->
            case Map.lookup key fields of
                 Just x -> return x
                 _ -> typeError p $ "invalid keyword " ++ show key ++ " for record " ++ (show t)
         _ -> typeError p "invalid lookup operation"

typeOf (TLookup p _ _) = typeError p "invalid lookup operation"

typeOf (TAbs _ x tyT1 t2) = do
    withTmpStateT (addVar x tyT1) $ do
        tyT2 <- typeOf t2
        return $ TyArrow tyT1 (typeShift (-1) tyT2)

typeOf (TApp p t1 t2) = do
    tyT1 <- typeOf t1
    tyT2 <- typeOf t2
    tyT1' <- simplifyType tyT1
    case tyT1' of
         (TyArrow tyT11 tyT12) -> do
            x <- typeEq tyT2 tyT11
            if x
            then return tyT12
            else typeError p $ "incorrect application of abstraction " ++ show tyT2 ++ " to " ++ show tyT11
         _ -> typeError p $ "incorrect application " ++ show tyT1 ++ " and " ++ show tyT2

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (typeEq ty1 TyBool)
            (typeError p $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty1 ++ ")")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    unlessM (typeEq ty2 ty3)
           (typeError p $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")")
    return ty2

typeOf (TZero _) = return TyNat

typeOf (TSucc p t) = do
    ty <- typeOf t
    unlessM (typeEq ty TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TPred p t) = do
    ty <- typeOf t
    unlessM (typeEq ty TyNat) (argumentError p TyNat ty)
    return TyNat

typeOf (TIsZero p t) = do
    ty <- typeOf t
    unlessM (typeEq ty TyNat) (argumentError p TyNat ty)
    return TyBool

typeOf (TTimesFloat p t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    unlessM (typeEq ty1 TyFloat) (argumentError p TyFloat ty1)
    unlessM (typeEq ty2 TyFloat) (argumentError p TyFloat ty2)
    return TyFloat

typeOf (TCase p v branches) = do
    v' <- typeOf v
    ty' <- simplifyType v'
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

         x -> (typeError p $ "Invalid context for case statement " ++ show x)

typeOf (TTag p key t1 ty) = do
    ty1 <- typeOf t1
    ty' <- simplifyType ty
    case ty' of
         TyVariant tys ->
            case Map.lookup key tys of
                 Just x -> do
                    whenM (typeEq x ty') (typeError p "field does not have expected type")
                    return ty
                 _ -> typeError p $ "label " ++ key ++ " not found"
         _ -> typeError p $ "Annotation is not a variant type : " ++ show ty1

typeOf (TLet _ x t1 t2) = do
    ty1 <- typeOf t1
    withTmpStateT (addVar x ty1) $ do
        ty2 <- typeOf t2
        return $ typeShift (-1) ty2

typeOf (TUnit _) = return TyUnit

typeOf (TPair _ t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    return $ TyProduct ty1 ty2

typeOf (TFix p t1) = do
    tyT1 <- typeOf t1
    tyT1' <- simplifyType tyT1
    case tyT1' of
        (TyArrow tyT11 tyT12) -> do
            unlessM (typeEq tyT11 tyT12) (typeError p $ "result of body not compatible with domain " ++ show tyT11 ++ " and " ++ show tyT12)
            return tyT12
        _ -> typeError p  "arrow type expected"

whenM :: Monad m => m Bool -> m () -> m ()
whenM p s = do
    x <- p
    when x s

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM p s = do
    x <- p
    unless x s

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

argumentError :: SourcePos -> Type -> Type -> Eval a
argumentError p expected actual = typeError p message
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
