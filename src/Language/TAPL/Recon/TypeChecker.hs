{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Language.TAPL.Recon.TypeChecker where

import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Context
import Language.TAPL.Recon.TypeReconstructor

class LCTypeChecker c where
    typeOf :: c Term -> Either (c EvaluationError) (c Type)

instance LCTypeChecker Context where
    typeOf :: Context Term -> Either (Context EvaluationError) (Context Type)
    typeOf (Context names vars constraints t) = do
        case reconstruct names vars t of
            Right (ty, vars', newConstraints) ->
                case unify (newConstraints ++ constraints) of
                    Right constraints'' ->
                        let ty' = applySubst constraints'' ty in
                        return $ Context names vars' constraints'' ty'
                    Left e -> Left $ Context names vars constraints e
            Left e -> Left $ Context names vars constraints e

applySubst :: [Constraint] -> Type -> Type
applySubst constraints tyT = foldl f tyT (reverse constraints)
    where f tyS ((TyID tyX), tyC2) = substInType tyX tyC2 tyS

substInType :: String -> Type -> Type -> Type
substInType tyX tyT (TyArrow tyS1 tyS2) = TyArrow (substInType tyX tyT tyS1) (substInType tyX tyT tyS2)
substInType tyX tyT TyNat = TyNat
substInType tyX tyT TyBool = TyBool
substInType tyX tyT (TyID s) | s == tyX = tyT
substInType tyX tyT (TyID s) = TyID s

substInConstraint :: String -> Type -> [Constraint] -> [Constraint]
substInConstraint tyX tyT cs = (uncurry f) <$> cs
                         where f tyS1 tyS2 = (substInType tyX tyT tyS1, substInType tyX tyT tyS2)

occursInt :: String -> Type -> Bool
occursInt tyX (TyArrow tyT1 tyT2) = (occursInt tyX tyT1) || (occursInt tyX tyT2)
occursInt tyX TyNat = False
occursInt tyX TyBool = False
occursInt tyX (TyID s) = s == tyX

unify :: [Constraint] -> Either EvaluationError [Constraint]
unify [] = return []

unify (((TyID x), (TyID y)):xs) | x == y = unify xs
unify cs@((tyS, (TyID tyX)):xs) | occursInt tyX tyS = Left $ CircularConstrains cs
unify cs@(((TyID tyX), tyT):xs) | occursInt tyX tyT = Left $ CircularConstrains cs

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
unify cs@((tyS, tyT):xs) = Left $ UnresolvedConstraints cs
