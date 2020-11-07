module Language.TAPL.FullError.TypeChecker (typeOf) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Helpers (unlessM, withTmpStateT)
import Language.TAPL.FullError.Types
import Language.TAPL.FullError.Context

typeOf :: Term -> Eval Type
typeOf (TTrue _) = return TyBool
typeOf (TFalse _) = return TyBool

typeOf (TVar p v _) = do
    n <- get
    case getBinding n v of
         (Just (VarBind ty)) -> return ty
         (Just x) -> typeError p $ "wrong kind of binding for variable (" ++ show x ++ " " ++ show n ++ " " ++ show v ++ ")"
         Nothing -> typeError p "var type error"

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
            unlessM (tyT2 <: tyT11)
                    (typeError p $ "incorrect application of abstraction " ++ show tyT2 ++ " to " ++ show tyT11)
            return tyT12
         TyBot -> return TyBot
         _ -> typeError p $ "arrow type expected"

typeOf (TIf p t1 t2 t3) = do
    ty1 <- typeOf t1
    unlessM (ty1 <: TyBool)
            (typeError p $ "guard of condition have not a " ++ show TyBool ++  " type (" ++ show ty1 ++ ")")
    ty2 <- typeOf t2
    ty3 <- typeOf t3
    joinTypes ty2 ty3

typeOf (TError _) = return TyBot
typeOf (TTry _ t1 t2) = do
    ty1 <- typeOf t1
    ty2 <- typeOf t2
    joinTypes ty1 ty2

typeError :: SourcePos -> String -> Eval a
typeError p message = lift $ throwE $ show p ++ ":" ++ message

typeEq :: Type -> Type -> Eval Bool
typeEq ty1 ty2 = do
    ty1' <- simplifyType ty1
    ty2' <- simplifyType ty2
    n <- get
    case (ty1', ty2') of
        (TyTop, TyTop) -> return True
        (TyBot, TyBot) -> return True
        (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> typeEq tyS1 tyT1
                                                       <*> typeEq tyS2 tyT2
        (TyBool, TyBool) -> return True

        (TyVar i _, _) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x ty2'
                _ -> return False

        (_, TyVar i _) | isTypeAbb n i -> do
            case (getTypeAbb n i) of
                Just x -> typeEq x ty1'
                _ -> return False

        (TyVar i _, TyVar j _) | i == j -> return True
        _ -> return False

(<:) :: Type -> Type -> Eval Bool
(<:) tyS tyT = do
    tyS' <- simplifyType tyS
    tyT' <- simplifyType tyT
    x <- typeEq tyS' tyT'
    if x
    then return True
    else case (tyS', tyT) of
              (_, TyTop) -> return True
              (TyBot, _) -> return False
              (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) -> (&&) <$> (tyS1 <: tyT1) <*> (tyS2 <: tyT2)
              _ -> return False

joinTypes :: Type -> Type -> Eval Type
joinTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyT
    else do y <- tyT <: tyS
            if y
            then return tyS
            else do tyS' <- simplifyType tyS
                    tyT' <- simplifyType tyT
                    case (tyS',tyT') of
                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) ->
                            TyArrow <$> meetTypes tyS1 tyT1 <*> joinTypes tyS2 tyT2
                         _ -> return TyTop

meetTypes :: Type -> Type -> Eval Type
meetTypes tyS tyT = do
    x <- tyS <: tyT
    if x
    then return tyT
    else do y <- tyT <: tyS
            if y
            then return tyS
            else do tyS' <- simplifyType tyS
                    tyT' <- simplifyType tyT
                    case (tyS',tyT') of
                         (TyArrow tyS1 tyS2, TyArrow tyT1 tyT2) ->
                            TyArrow <$> joinTypes tyS1 tyT1 <*> meetTypes tyS2 tyT2
                         _ -> return TyBot

computeType :: Type -> Eval (Maybe Type)
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
