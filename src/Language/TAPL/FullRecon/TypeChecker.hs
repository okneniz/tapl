module Language.TAPL.FullRecon.TypeChecker where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Context
import Language.TAPL.FullRecon.Types
import Language.TAPL.FullRecon.Context
import Language.TAPL.FullRecon.TypeReconstructor

typeOf :: Term -> Eval Type
typeOf t = do
  c1 <- getConstraints
  ty <- reconstruct t
  c2 <- getConstraints
  putConstraints $ c1 <> c2
  unify *> applySubst ty

applySubst :: Type -> Eval Type
applySubst tyT = foldl f tyT <$> (reverse <$> getConstraints)
    where f tyS ((TyID tyX), tyC2) = substInType tyX tyC2 tyS

substInType :: String -> Type -> Type -> Type
substInType tyX tyT (TyArrow tyS1 tyS2) = TyArrow (substInType tyX tyT tyS1) (substInType tyX tyT tyS2)
substInType _ _ TyNat = TyNat
substInType _ _ TyBool = TyBool
substInType tyX tyT (TyID s) | s == tyX = tyT
substInType _ _ (TyID s) = TyID s

substInConstraint :: String -> Type -> [Constraint] -> [Constraint]
substInConstraint tyX tyT cs = (uncurry f) <$> cs
                         where f tyS1 tyS2 = (substInType tyX tyT tyS1, substInType tyX tyT tyS2)

occursInt :: String -> Type -> Bool
occursInt tyX (TyArrow tyT1 tyT2) = (occursInt tyX tyT1) || (occursInt tyX tyT2)
occursInt _ TyNat = False
occursInt _ TyBool = False
occursInt tyX (TyID s) = s == tyX

unify :: Eval ()
unify = getConstraints >>= f >>= putConstraints
  where f [] = return []
        f (((TyID x), (TyID y)):xs) | x == y = f xs
        f cs@((tyS, (TyID tyX)):_) | occursInt tyX tyS = lift $ throwE $ show $ CircularConstrains cs
        f cs@(((TyID tyX), tyT):_) | occursInt tyX tyT = lift $ throwE $ show $ CircularConstrains cs
        f ((tyS, (TyID tyX)):xs) = (<>) [(TyID tyX, tyS)] <$> f (substInConstraint tyX tyS xs)
        f (((TyID tyX), tyT):xs) = (<>) [(TyID tyX, tyT)] <$> f (substInConstraint tyX tyT xs)
        f ((TyNat, TyNat):xs) = f xs
        f ((TyBool, TyBool):xs) = f xs
        f ((TyArrow tyS1 tyS2, TyArrow tyT1 tyT2):xs) = f ((tyS1,tyT1):(tyS2,tyT2):xs)
        f cs = lift $ throwE $ show $ UnresolvedConstraints cs

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk _ TyBool = TyBool
                     walk _ TyNat = TyNat
                     walk c (TyVar x n) = onVar c x n
                     walk _ (TyID x) = TyID x
                     walk c (TyArrow ty1 ty2) = TyArrow (walk c ty1) (walk c ty2)

typeShiftAbove :: Depth -> VarName -> Type -> Type
typeShiftAbove d s ty = typeMap onVar s ty
                  where onVar c name depth | name >= c = TyVar (name + d) (depth + d)
                        onVar _ name depth = TyVar name (depth + d)

typeShift :: VarName -> Type -> Type
typeShift d ty = typeShiftAbove d 0 ty

typeSubstitution :: VarName -> Type -> Type -> Type
typeSubstitution j s ty = typeMap onVar 0 ty
                    where onVar c name _ | name == j + c = typeShift c s
                          onVar _ name depth = TyVar name depth

typeSubstitutionTop :: Type -> Type -> Type
typeSubstitutionTop s ty = typeShift (-1) (typeSubstitution 0 (typeShift 1 s) ty)
