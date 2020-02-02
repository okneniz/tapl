{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.Equirec.TypeChecker (typeOf, typeShiftAbove) where
import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Context

import Control.Monad (liftM)
import Data.Either (isRight, isLeft)
import Data.List (intercalate, all, nub, (\\), find, sortBy)
import Data.Ord (comparing)

import Prelude hiding (lookup)
import qualified Prelude (lookup)

class LCContext c => TypeChecker c where
  typeFromContext :: c -> VarName -> Maybe Binding
  isCorrect :: c -> Bool
  typeOf :: c -> Either TypeError Type

instance TypeChecker (EquirecContext Term) where
  typeFromContext context name = liftM snd $ pickVar context name
  isCorrect c = isRight $ typeOf c

  typeOf c@(EquirecContext n v@(TVar info varname depth)) =
      let ty = typeFromContext c varname
      in case ty of
              Just (VarBind ty') -> return ty'
              Just x -> Left $ TypeMissmatch info $ "wrong kind of binding for variable"
              Nothing -> Left $ TypeMissmatch info $ "var type error"

  typeOf c@(EquirecContext n (TApp info t1 t2)) = do
      ty1 <- typeOf $ EquirecContext n $ t1
      ty2 <- typeOf $ EquirecContext n $ t2
      case (simplifyType c ty1)of
           (TyArrow ty1' ty2') ->
             if ty2 <: ty1'
             then return ty2'
             else
                Left $ TypeMissmatch info $ "incorrect application of abstraction " ++ (show $ EquirecContext n ty2) ++ " to " ++ (show $ EquirecContext n ty1')
           x -> Left $ TypeMissmatch info $ "incorrect application " ++ (show $ EquirecContext n ty1) ++ " and " ++ (show $ EquirecContext n ty2)

  typeOf c@(EquirecContext n (TAbs _ name ty t)) = do
      let c' = bind (EquirecContext n t) name (VarBind ty)
      ty' <- typeOf c'
      return $ TyArrow ty (typeShift (-1) ty')

computeType :: a -> Type -> Maybe Type
computeType c tyS@(TyRec x tyS1) = Just $ typeSubstitutionTop tyS tyS1
computeType _ _ = Nothing

simplifyType :: a -> Type -> Type
simplifyType c ty = case computeType c ty of
                         Just x -> simplifyType c x
                         _ -> ty

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyVar x n) = onVar c x n
                     walk c (TyRec x ty1) = TyRec x (walk (c + 1) ty1)
                     walk c (TyID x) = TyID x
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)

typeShiftAbove :: Depth -> VarName -> Type -> Type
typeShiftAbove d c ty = typeMap onVar c ty
                  where onVar c name depth | name >= c = TyVar (name + d) (depth + d)
                        onVar c name depth = TyVar name (depth + d)

typeShift :: VarName -> Type -> Type
typeShift d ty = typeShiftAbove d 0 ty

typeSubstitution :: VarName -> Type -> Type -> Type
typeSubstitution j s ty = typeMap onVar 0 ty
                    where onVar c name depth | name == j + c = typeShift c s
                          onVar c name depth = TyVar name depth

typeSubstitutionTop :: Type -> Type -> Type
typeSubstitutionTop s ty = typeShift (-1) (typeSubstitution 0 (typeShift 1 s) ty)
