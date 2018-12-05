{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module TAPL.FullSimple.TypeChecker where

import TAPL.FullSimple.Types
import TAPL.FullSimple.Context
import Control.Monad (liftM)

import Prelude hiding (abs, succ, pred)
import Data.List (intercalate, all, nub, (\\), find, sortBy)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

class LCContext c => TypeChecker c where
  typeFromContext :: c -> VarName -> Maybe Binding
  isCorrect :: c -> Bool
  typeOf :: c -> Either TypeError Type

instance TypeChecker (FullSimpleContext Term) where
  typeFromContext context name = liftM snd $ pickVar context name
  isCorrect c = isRight $ typeOf c

  typeOf (FullSimpleContext _ (TTrue _)) = return TyBool
  typeOf (FullSimpleContext _ (TFalse _)) = return TyBool
  typeOf (FullSimpleContext _ (TString _ _)) = return TyString
  typeOf (FullSimpleContext _ (TUnit _)) = return TyUnit
  typeOf (FullSimpleContext _ (TZero _)) = return TyNat
  typeOf (FullSimpleContext _ (TInt _ _)) = return TyInt

  typeOf (FullSimpleContext n (TSucc info t)) = do
      ty <- typeOf $ FullSimpleContext n t
      case ty of
          TyNat -> return TyNat
          ty -> Left $ TypeMissmatch info $ "argument of succ is not a natural number (" ++ show ty ++ ")"

  typeOf (FullSimpleContext n (TPred info t)) = do
      ty <- typeOf $ FullSimpleContext n t
      case ty of
         TyNat -> return TyNat
         ty -> Left $ TypeMissmatch info $ "argument of pred is not a natural number (" ++ show ty ++ ")"

  typeOf (FullSimpleContext n (TIsZero info t)) = do
      ty <- typeOf $ FullSimpleContext n t
      case ty of
        TyNat -> return TyBool
        ty -> Left $ TypeMissmatch info $ "argument of zero? is not a natural number (" ++ show ty ++ ")"

  typeOf (FullSimpleContext n (TIf info t1 t2 t3)) = do
      ty1 <- typeOf $ FullSimpleContext n t1
      ty2 <- typeOf $ FullSimpleContext n t2
      ty3 <- typeOf $ FullSimpleContext n t3
      case ty1 of
           TyBool -> if ty2 == ty3
                     then return ty2
                     else Left $ TypeMissmatch info $ "branches of condition have different types (" ++ show ty2 ++ " and " ++ show ty3 ++ ")"
           ty -> Left $ TypeMissmatch info $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty ++ ")"

  typeOf c@(FullSimpleContext n v@(TVar info varname depth)) =
      let ty = typeFromContext c varname
      in case ty of
              Just (VarBind ty') -> return ty'
              Just x -> Left $ TypeMissmatch info $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
              Nothing -> Left $ TypeMissmatch info $ "var type error"

  typeOf (FullSimpleContext n (TApp info t1 t2)) = do
      ty1 <- typeOf $ FullSimpleContext n $ t1
      ty2 <- typeOf $ FullSimpleContext n $ t2
      case ty1 of
           (TyArrow ty1' ty2') -> if ty2 <: ty1'
                                  then return ty2'
                                  else Left $ TypeMissmatch info $ "incorrect application of abstraction " ++ show ty2 ++ " to " ++ show ty1'
           TyBot -> return TyBot
           x -> Left $ TypeMissmatch info $ "incorrect application " ++ show ty1 ++ " and " ++ show ty2

  typeOf c@(FullSimpleContext n (TAbs _ name ty t)) = do
      let t' = bind (FullSimpleContext n t) name (VarBind ty)
      ty' <- typeOf t'
      return $ TyArrow ty ty'

  typeOf (FullSimpleContext _ (TFloat info x)) = Right TyFloat

  typeOf c@(FullSimpleContext n (TPair info t1 t2)) = do
    ty1 <- typeOf $ c `withTerm` t1
    ty2 <- typeOf $ c `withTerm` t2
    return $ TyProduct ty1 ty2

  typeOf c@(FullSimpleContext n v@(TRecord info fields)) = do
      let f t = typeOf $ c `withTerm` t
      let tyFields = (\(k, v) -> (k, f v)) <$> fields
      let check = all (\(k, v) -> isRight v) tyFields
      let tys = fmap (\(k, (Right t)) -> (k,t)) tyFields
      let err = head $ fmap snd $ filter (\(_, v) -> isLeft v) tyFields
      if check
      then return $ TyRecord tys
      else err

  typeOf c@(FullSimpleContext n (TLookup _ t (TInt info i))) =
      case (typeOf $ c `withTerm` t, i) of
           ((Right (TyProduct ty _)), 0) -> Right ty
           ((Right (TyProduct _ ty)), 1) -> Right ty
           ((Right (TyProduct _ ty)), _) -> Left $ TypeMissmatch info "invalid index for pair"
           (x, _) | isRight x -> Left $ TypeMissmatch info "invalid lookup operation"
           (x, _) -> x

  typeOf c@(FullSimpleContext n (TLookup _ t (TKeyword info key))) =
      case typeOf $ c `withTerm` t of
           (Right (TyRecord fields)) -> case Prelude.lookup key fields of
                                             Just ty -> return ty
                                             _ -> Left $ TypeMissmatch info $ "invalid keyword " ++ show key ++ " for record " ++ (show $ c `withTerm` t)
           x | isRight x -> Left $ TypeMissmatch info "invalid lookup operation"
           x -> x

  typeOf c@(FullSimpleContext n (TLookup info t k)) = do
      Left $ TypeMissmatch info "invalid lookup operation"

  typeOf c@(FullSimpleContext n (TLet info v t1 t2)) = do
      ty1 <- typeOf $ FullSimpleContext n t1
      ty2 <- typeOf $ (bind c v (VarBind ty1)) `withTerm` t2
      return ty2

  typeOf c@(FullSimpleContext n (TAscribe info t ty)) = do
      ty' <- typeOf $ c `withTerm` t
      if ty' <: ty
      then return ty
      else Left $ TypeMissmatch info "body of as-term does not have the expected type"

  typeOf c@(FullSimpleContext n (TFix info t1)) = do
    tyT1 <- typeOf $ c `withTerm` t1
    case tyT1 of
         (TyArrow tyT11 tyT12) | tyT12 <: tyT11 -> return tyT12
         (TyArrow tyT11 tyT12) -> Left $ TypeMissmatch info  "result of body not compatible with domain"
         _ -> Left $ TypeMissmatch info  "arrow type expected"

  typeOf c@(FullSimpleContext _ (TCase info v@(TTag _ key _ _) branches)) = do
    ty' <- typeOf $ c `withTerm` v

    case ty' of
      TyVariant fields -> do
        _ <- checkInvalidCaseBranches branches fields
        _ <- checkAbsentCaseBranches branches fields
        _ <- checkInvalidBranchesTypes branches fields
        _ <- checkValidBranchesTypes branches fields
        branch <- matchedBranch key branches
        field <- matchedVariantField key fields
        vty' <- typeOfBranch branch field
        return vty'
    where variantKey = fst
          branchKey (x, _, _) = x
          variantKeys fs = variantKey <$> fs
          branchesKeys bs = branchKey <$> bs
          typeOfBranch (_, varName, t) (_, vty) = typeOf $ (bind c varName (VarBind vty)) `withTerm` t
          branchesTypes bs fs = (\(br,fi) -> typeOfBranch br fi) <$> zip (sortBy (comparing branchKey) bs)
                                                                         (sortBy (comparing variantKey) fs)

          matchedBranch k caseBranches = do
            case find (\x -> k == (branchKey x)) caseBranches of
              Just branch -> return branch
              _ -> Left $ TypeMissmatch info $ "Missmatch case branch with key " ++ show k

          matchedVariantField k variantFields = do
            case Prelude.lookup k variantFields of
              Just field -> return (k, field)
              _ -> Left $ TypeMissmatch info $ "Missmatch variant with key " ++ show k

          checkInvalidCaseBranches bs fs = do
            case (branchesKeys bs) \\ (variantKeys fs) of
              [] -> Right []
              fs -> Left $ TypeMissmatch info $ "Invalid case branch " ++ intercalate ", " fs

          checkAbsentCaseBranches bs fs = do
            case (variantKeys fs) \\ (branchesKeys bs) of
              [] -> Right []
              fs -> Left $ TypeMissmatch info $ "Absent case branch " ++ intercalate ", " fs

          checkInvalidBranchesTypes bs fs = do
            case filter isLeft $ (branchesTypes bs fs) of
              [] -> Right []
              fs -> Left $ TypeMissmatch info $ "Invalid case branch types " ++ intercalate ", " (show <$> fs)

          checkValidBranchesTypes bs fs = do
            case nub $ fromRight <$> (branchesTypes bs fs) of
              l | (length l) > 1 -> Left $ TypeMissmatch info $ "Case branches have different types " ++ intercalate ", " (show <$> l)
              _ -> Right []

  typeOf c@(FullSimpleContext _ (TTag info key t ty)) = do
    ty' <- typeOf $ c `withTerm` t
    case ty of
        TyVariant tys -> case Prelude.lookup key tys of
                              Just x -> if x == ty'
                                        then return ty
                                        else Left $ TypeMissmatch info $ "field does not have expected type"
                              _ -> Left $ TypeMissmatch info $ "label " ++ key ++ " not found"
        _ -> Left $ TypeMissmatch info $ "Annotation is not a variant type"

  typeOf x = error $ show x ++ " ??"

fromRight (Right x) = x
fromRight (Left x) = undefined

fromLeft (Right x) = undefined
fromLeft (Left x) = x
