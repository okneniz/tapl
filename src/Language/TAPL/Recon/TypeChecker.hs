module Language.TAPL.Recon.TypeChecker where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Context
import Language.TAPL.Recon.TypeReconstructor

typeOf :: Term -> Eval Type
typeOf t = do
    ty <- reconstruct t
    (vi, cs) <- lift $ lift $ get
    case unify cs of
        Right cs' -> do
            lift $ lift $ put (vi, cs')
            return $ applySubst cs' ty
        Left x -> lift $ throwE $ show x

applySubst :: [Constraint] -> Type -> Type
applySubst constraints tyT = foldl f tyT (reverse constraints)
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

unify :: [Constraint] -> Either TypeError [Constraint]
unify [] = return []

unify (((TyID x), (TyID y)):xs) | x == y = unify xs
unify cs@((tyS, (TyID tyX)):_) | occursInt tyX tyS = Left $ CircularConstrains cs
unify cs@(((TyID tyX), tyT):_) | occursInt tyX tyT = Left $ CircularConstrains cs

unify ((tyS, (TyID tyX)):xs) = do
    case unify (substInConstraint tyX tyS xs) of
        Right cs -> Right $ cs ++ [(TyID tyX, tyS)]
        Left e -> Left e

unify (((TyID tyX), tyT):xs) = do
    case unify (substInConstraint tyX tyT xs) of
        Right cs -> Right $ cs ++ [(TyID tyX, tyT)]
        Left e -> Left e

unify ((TyNat, TyNat):xs) = unify xs
unify ((TyBool, TyBool):xs) = unify xs
unify ((TyArrow tyS1 tyS2, TyArrow tyT1 tyT2):xs) = unify ((tyS1,tyT1):(tyS2,tyT2):xs)
unify cs = Left $ UnresolvedConstraints cs

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
