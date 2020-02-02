{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Language.TAPL.Recon.TypeReconstructor where
import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Context

class (LCNames n) => LCTypeReconstructor n where
    reconstruct :: n -> UVarGen -> Term -> Either EvaluationError (Type, UVarGen, [Constraint])

instance LCTypeReconstructor Names where
    reconstruct :: Names -> UVarGen -> Term -> Either EvaluationError (Type, UVarGen, [Constraint])

    reconstruct names uvar (TVar info varName _) =
        case varType names varName of
            Just tyT -> Right (tyT, uvar, [])
            Nothing -> Left $ WrongKindOfBinding info varName

    reconstruct names uvar (TAbs _ x tyT1 t2) = do
        let names' = bind names x (VarBind(tyT1))
        (tyT2, uvar2, constr2) <- reconstruct names' uvar t2
        return ((TyArrow tyT1 tyT2), uvar2, constr2)

    reconstruct names uvar (TApp _ t1 t2) = do
        (tyT1, uvar1, constraints1) <- reconstruct names uvar t1
        (tyT2, uvar2, constraints2) <- reconstruct names uvar1 t2
        let (UVar tyX uvar') = uvar2 ()
        let newConstraints = [(tyT1, TyArrow tyT2 (TyID tyX))]
        return $ (TyID tyX, uvar', constraints1 ++ constraints2 ++ newConstraints)

    reconstruct names uvar (TZero _) = return (TyNat, uvar, [])

    reconstruct names uvar (TSucc _ t1) = do
        (tyT1,uvar1,constr1) <- reconstruct names uvar t1
        return (TyNat, uvar1, (tyT1,TyNat):constr1)

    reconstruct names uvar (TPred _ t1) = do
        (tyT1,uvar1,constr1) <- reconstruct names uvar t1
        return (TyNat, uvar1, (tyT1,TyNat):constr1)

    reconstruct names uvar (TIsZero _ t1) = do
        (tyT1,uvar1,constr1) <- reconstruct names uvar t1
        return (TyBool, uvar1, (tyT1,TyNat):constr1)

    reconstruct names uvar (TTrue _) = return (TyBool, uvar, [])
    reconstruct names uvar (TFalse _) = return (TyBool, uvar, [])

    reconstruct names uvar (TIf _ t1 t2 t3) = do
        (tyT1,uvar1,constr1) <- reconstruct names uvar t1
        (tyT2,uvar2,constr2) <- reconstruct names uvar1 t2
        (tyT3,uvar3,constr3) <- reconstruct names uvar2 t3
        let newconstr = [(tyT1,TyBool), (tyT2,tyT3)]
        return (tyT3, uvar3, (newconstr ++ constr1 ++ constr2 ++ constr3))
