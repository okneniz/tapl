{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TAPL.FullEquirec.TypeChecker where

import TAPL.FullEquirec.Types
import TAPL.FullEquirec.Names

import Control.Monad (liftM)
import Prelude hiding (abs, succ, pred)
import Data.List (intercalate, all, nub, (\\), find, sortBy, findIndex)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Data.Function (on)

class (LCNames n) => LCTypeChecker n where
  typeOf :: n -> Term -> Either TypeError Type
  typeEq :: n -> Type -> Type -> Bool

data TypeError = ArgumentError Info Type Type
               | InvalidGuardType Info Type
               | VariableNotFound Info VarName
               | IncorrectApplication Info Term Term
               | InvalidIndex Info Integer
               | InvalidKeyword Info Term String
               | UnexpectedOperation Info Term
               | IncompatibilityType Info Type String
               | NotImplementedCaseBranches Info [String]
               | MissmatchVariantKey Info String
               | InvalidCaseBranches Info [String]
               | InvalidCaseBranchesTypes Info [Either TypeError Type]
               | CaseBranchesHaveDifferentTypes Info [Type]
               | DifferentTypesInBranches Info [Type]
               | UnexpectedType Info Type
               | TypesMissmatch Info Type Type
               | LabelNotFound Info String
               | InvalidFixation Info
               | WrongKindOfBinding Info Binding
               | InvalidAST
               deriving (Show)

instance LCTypeChecker Names where
  typeOf _ (TString _ _) = return TyString
  typeOf _ (TFloat _ _) = return TyFloat
  typeOf _ (TTrue _) = return TyBool
  typeOf _ (TFalse _) = return TyBool

  typeOf n (TVar info varName _) =
    case getBinding n varName of
         (Just (VarBind ty)) -> return ty
         (Just x) -> Left $ WrongKindOfBinding info x
         Nothing -> Left $ VariableNotFound info varName

  typeOf n (TAscribe info t1 ty) = do
    ty1 <- typeOf n t1
    if typeEq n ty ty1
    then Right ty
    else Left $ TypesMissmatch info ty ty1

  typeOf n (TRecord _ fields) = do
    let tyFields = (\(s, t) -> (s, typeOf n t)) <$> fields
    let tys = fmap (\(k, (Right t)) -> (k,t)) tyFields
    let err = head $ fmap snd $ filter (\(_, v) -> isLeft v) tyFields
    if all (\(k, v) -> isRight v) tyFields
    then return $ TyRecord tys
    else err

  typeOf n (TLookup info t1 (TKeyword _ k)) = do
    ty <- typeOf n t1
    case simplifyType n ty of
         (TyRecord fields) -> case Prelude.lookup k fields of
                                   Just ty -> Right ty
                                   Nothing -> Left $ LabelNotFound info k
         ty -> Left $ UnexpectedType info ty

  typeOf n (TLookup info t (TInt _ i)) = do
    ty <- typeOf n t
    case simplifyType n ty of
         (TyProduct ty1 ty2) | i == 0 -> Right ty1
         (TyProduct ty1 ty2) | i == 1 -> Right ty2
         (TyProduct ty1 ty2) -> Left $ InvalidIndex info i
         ty -> Left $ UnexpectedType info ty

  typeOf n (TAbs _ x tyT1 t2) = do
    tyT2 <- typeOf (addVar n x tyT1) t2
    return $ TyArrow tyT1 (typeShift (-1) tyT2)

  typeOf n (TApp info t1 t2) = do
    tyT1 <- typeOf n t1
    tyT2 <- typeOf n t2

    case simplifyType n tyT1 of
         z@(TyArrow tyT11 tyT12) | typeEq n tyT2 tyT11 -> Right tyT12
         z@(TyArrow tyT11 _) -> Left $ TypesMissmatch info tyT2 tyT11
         x -> Left $ UnexpectedType info x

  typeOf n (TIf info t1 t2 t3) = do
    ty1 <- typeOf n t1
    ty2 <- typeOf n t2
    ty3 <- typeOf n t3
    if typeEq n ty1 TyBool
    then if typeEq n ty2 ty3
         then Right ty2
         else Left $ DifferentTypesInBranches info [ty2, ty3]
    else Left $ InvalidGuardType info ty1

  typeOf _ (TZero _) = return TyNat

  typeOf n (TSucc info t) = do
    ty <- typeOf n t
    if typeEq n ty TyNat
    then Right TyNat
    else Left $ UnexpectedType info ty

  typeOf n (TPred info t) = do
    ty <- typeOf n t
    if typeEq n ty TyNat
    then Right TyNat
    else Left $ UnexpectedType info ty

  typeOf n (TIsZero info t) = do
    ty <- typeOf n t
    if typeEq n ty TyNat
    then Right TyBool
    else Left $ UnexpectedType info ty

  typeOf n (TTimesFloat info t1 t2) = do
    ty1 <- typeOf n t1
    ty2 <- typeOf n t2
    if (typeEq n ty1 TyFloat)
    then if (typeEq n ty2 TyFloat)
         then Right TyFloat
         else Left $ UnexpectedType info ty2
    else Left $ UnexpectedType info ty1

  typeOf n (TCase info v branches) = do
    ty' <- typeOf n v

    case simplifyType n ty' of
      TyVariant fields -> do
        _ <- checkInvalidCaseBranches branches fields
        _ <- checkAbsentCaseBranches branches fields
        _ <- checkInvalidBranchesTypes branches fields
        _ <- checkValidBranchesTypes branches fields
        vty' <- head $ branchesTypes branches fields
        return vty'
    where variantKey = fst
          branchKey (x, _, _) = x
          variantKeys fs = variantKey <$> fs
          branchesKeys bs = branchKey <$> bs
          typeOfBranch (_, name, t) (_, vty) = do
            x <- typeOf (addVar n name vty) t
            return $ typeShift (-1) x
          branchesTypes bs fs = (\(br,fi) -> typeOfBranch br fi) <$> zip (sortBy (comparing branchKey) bs)
                                                                         (sortBy (comparing variantKey) fs)

          matchedBranch k caseBranches = do
            case find (\x -> k == (branchKey x)) caseBranches of
              Just branch -> return branch
              _ -> Left $ MissmatchVariantKey info k

          matchedVariantField k variantFields = do
            case Prelude.lookup k variantFields of
              Just field -> return (k, field)
              _ -> Left $ MissmatchVariantKey info k

          checkInvalidCaseBranches bs fs = do
            case (branchesKeys bs) \\ (variantKeys fs) of
              [] -> Right []
              fs -> Left $ InvalidCaseBranches info fs

          checkAbsentCaseBranches bs fs = do
            case (variantKeys fs) \\ (branchesKeys bs) of
              [] -> Right []
              fs -> Left $ NotImplementedCaseBranches info fs

          checkInvalidBranchesTypes bs fs = do
            case filter isLeft $ (branchesTypes bs fs) of
              [] -> Right []
              fs -> Left $ InvalidCaseBranchesTypes info fs

          checkValidBranchesTypes bs fs = do
            tys <- sequenceA $ branchesTypes bs fs
            let hd = head tys
            let tl = tail tys
            if all (\x -> typeEq n hd x) tl
            then Right []
            else Left $ CaseBranchesHaveDifferentTypes info []

  typeOf n (TTag info key t ty) = do
    ty' <- typeOf n t
    case simplifyType n ty of
        TyVariant tys -> case Prelude.lookup key tys of
                              Just x -> if typeEq n x ty'
                                        then return ty
                                        else Left $ TypesMissmatch info ty' x
                              _ -> Left $ LabelNotFound info key
        x -> Left $ UnexpectedType info x

  typeOf n (TLet _ x t1 t2) = do
    ty1 <- typeOf n t1
    ty2 <- typeOf (addVar n x ty1) t2
    return $ typeShift (-1) ty2

  typeOf n (TUnit _) = return TyUnit

  typeOf n (TPair info t1 t2) = do
    ty1 <- typeOf n t1
    ty2 <- typeOf n t2
    return $ TyProduct ty1 ty2

  typeOf n (TFix info t1) = do
    tyT1 <- typeOf n t1
    case simplifyType n tyT1 of
        x@(TyArrow tyT11 tyT12) ->
            if typeEq n tyT11 tyT12
            then Right tyT12
            else Left $ InvalidFixation info
        x -> Left $ UnexpectedType info x

  typeOf n (TBind _ _ _) = Right TyUnit;
  typeOf n x = error $ show n ++ " -- " ++ show x

  typeEq names tyS tyT = typeEq' [] names tyS tyT
    where mem y [] = False
          mem y (x:xs) = (y == x) || (mem y xs)
          typeEq' seen ns ty1 ty2 =
            if mem (tyS,tyT) seen
            then True
            else typeEq'' [] ns ty1 ty2
          typeEq'' seen _ TyString TyString = True
          typeEq'' seen _ TyFloat TyFloat = True
          typeEq'' seen _ TyUnit TyUnit = True
          typeEq'' seen _ TyBool TyBool = True
          typeEq'' seen _ TyNat TyNat = True
          typeEq'' seen n (TyRec x tyS1) _ = typeEq' ((tyS, tyT):seen) n (typeSubstitutionTop tyS tyS1) tyT
          typeEq'' seen n _ (TyRec x tyT1) = typeEq' ((tyS, tyT):seen) n tyS (typeSubstitutionTop tyT tyT1)
          typeEq'' seen _ (TyID b1) (TyID b2) = b1 == b2

          typeEq'' seen n tyS@(TyVar i _) tyT | isTypeAbb n i =
            case (getTypeAbb n i) of
                Just x -> typeEq' seen n x tyT
                _ -> False

          typeEq'' seen n tyS tyT@(TyVar i _) | isTypeAbb n i =
            case (getTypeAbb n i) of
                Just x -> typeEq' seen n tyS x
                _ -> False

          typeEq'' seen n x@(TyVar _ i) y@(TyVar _ j) = i == j

          typeEq'' seen n (TyArrow tyS1 tyS2) (TyArrow tyT1 tyT2) = (typeEq' seen n tyS1 tyT1) && (typeEq' seen n tyS2 tyT2)

          typeEq'' seen n (TyRecord fields1) (TyRecord fields2) | (length fields1) /= (length fields2) = False
          typeEq'' seen n (TyRecord fields1) (TyRecord fields2) =
            let f = compare `on` fst
                x1 = sortBy f fields1
                x2 = sortBy f fields2
            in all (\((s1, ty1), (s2, ty2)) -> (s1 == s2) == (typeEq' seen n ty1 ty2)) $ zip x1 x2

          typeEq'' seen n (TyVariant fields1) (TyVariant fields2) | (length fields1) /= (length fields2) = False
          typeEq'' seen n (TyVariant fields1) (TyVariant fields2) =
            let f = compare `on` fst
                x1 = sortBy f fields1
                x2 = sortBy f fields2
            in all (\((s1, ty1), (s2, ty2)) -> (s1 == s2) == (typeEq n ty1 ty2)) $ zip x1 x2

          typeEq'' seen n (TyProduct t1 t2) (TyProduct t11 t12) = (typeEq' seen n t1 t11) && (typeEq' seen n t2 t12)

          typeEq'' seen _ _ _ = False

computeType :: Names -> Type -> Maybe Type
computeType _ ty@(TyRec x tyS) = Just $ typeSubstitutionTop ty tyS
computeType n (TyVar i _) | isTypeAbb n i = getTypeAbb n i
computeType _ _ = Nothing

simplifyType :: Names -> Type -> Type
simplifyType n ty = case computeType n ty of
                         Just x -> simplifyType n x
                         _ -> ty

