{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module TAPL.FullRecon.TypeChecker where

import TAPL.FullRecon.Types
import TAPL.FullRecon.Context
import TAPL.FullRecon.TypeReconstructor

class LCTypeChecker c where
    typeOf :: c Term -> Either (c EvaluationError) (c Type)

instance LCTypeChecker Context where
    typeOf :: Context Term -> Either (Context EvaluationError) (Context Type)
    typeOf (Context names vars constraints t) = do
        case reconstruct names vars t of
            Right (ty, vars', constraints_t) ->
                case unify (constraints ++ constraints_t) of
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

occursIn :: String -> Type -> Bool
occursIn tyX (TyArrow tyT1 tyT2) = (occursIn tyX tyT1) || (occursIn tyX tyT2)
occursIn tyX TyNat = False
occursIn tyX TyBool = False
occursIn tyX (TyID s) = s == tyX

unify :: [Constraint] -> Either EvaluationError [Constraint]
unify [] = return []
unify (((TyID x), (TyID y)):xs) | x == y = unify xs

unify cs@((tyS, (TyID tyX)):xs) | occursIn tyX tyS = Left $ CircularConstrains cs

unify ((tyS, (TyID tyX)):xs) = do
    case unify (substInConstraint tyX tyS xs) of
        Right cs -> Right $ cs ++ [(TyID tyX, tyS)]
        Left e -> Left e

unify cs@(((TyID tyX), tyT):xs) | occursIn tyX tyT = Left $ CircularConstrains cs

unify (((TyID tyX), tyT):xs) = do
    case unify (substInConstraint tyX tyT xs) of
        Right cs -> Right $ cs ++ [(TyID tyX, tyT)]
        Left e -> Left e

unify ((TyNat, TyNat):xs) = unify xs
unify ((TyBool, TyBool):xs) = unify xs
unify ((TyArrow tyS1 tyS2, TyArrow tyT1 tyT2):xs) = unify ((tyS1,tyT1):(tyS2,tyT2):xs)
unify cs@((tyS, tyT):xs) = Left $ UnresolvedConstraints cs
