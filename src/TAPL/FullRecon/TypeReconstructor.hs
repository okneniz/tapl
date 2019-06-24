{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module TAPL.FullRecon.TypeReconstructor where
import TAPL.FullRecon.Types
import TAPL.FullRecon.Context

class (LCNames n) => LCTypeReconstructor n where
    reconstruct :: n -> UVarGen -> Term -> Either EvaluationError (Type, UVarGen, [Constraint])

instance LCTypeReconstructor Names where
    reconstruct names uvar (TVar info varName _) =
        case varType names varName of
            Just tyT -> Right (tyT, uvar, [])
            Nothing -> Left $ WrongKindOfBinding info varName

    reconstruct names uvar (TAbs _ x (Just tyT1) t2) = do
        let names' = addVar names x tyT1
        (tyT2, uvar2, constr2) <- reconstruct names' uvar t2
        return ((TyArrow tyT1 tyT2), uvar2, constr2)

    reconstruct names uvar (TAbs _ x Nothing t2) = do
      let (UVar u uvar') = uvar ()
          tyX = (TyID u)
          names' = addVar names x tyX
      (tyT2,uvar'',constraints) <- reconstruct names' uvar' t2
      return $ (TyArrow tyX tyT2, uvar'', constraints)

    reconstruct names uvar (TApp _ t1 t2) = do
        (tyT1, uvar1, constraints1) <- reconstruct names uvar t1
        (tyT2, uvar2, constraints2) <- reconstruct names uvar1 t2
        let (UVar tyX uvar') = uvar2 ()
        let newConstraints = [(tyT1, TyArrow tyT2 (TyID tyX))]
        return $ (TyID tyX, uvar', newConstraints ++ constraints1 ++ constraints2)

    reconstruct names uvar (TLet _ x t1 t2) | isVal t1 =
        reconstruct names uvar $ termSubstitutionTop t1 t2

    reconstruct names uvar (TLet _ x t1 t2) = do
        (ty1, uvar', constraints') <- reconstruct names uvar t1
        (ty2, uvar'', constraints'') <- reconstruct (addVar names x ty1) uvar' t2
        return $ (ty2, uvar'', constraints' ++ constraints'')

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
