module Language.TAPL.Equirec.TypeChecker (typeOf, typeShiftAbove) where

import Control.Monad (liftM)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Context

type Inferred a = ExceptT TypeError (State LCNames) a
data TypeError = TypeMissmatch Info String

typeOf :: LCNames -> Term -> Either String Type
typeOf names term =
    case evalState (runExceptT (infer term)) names of
         Left x -> Left $ show x
         Right x -> return x

infer :: Term -> Inferred Type
infer (TVar info varname _) = do
    names <- lift get
    case liftM snd $ pickVar names varname of
         Just (VarBind ty') -> return ty'
         Just _ -> throwE $ TypeMissmatch info $ "wrong kind of binding for variable"
         Nothing -> throwE $ TypeMissmatch info $ "var type error"

infer (TApp info t1 t2) = do
    ty1 <- infer t1
    ty2 <- infer t2
    names <- lift get
    case simplifyType names ty1 of
         (TyArrow ty1' ty2') ->
             if ty2 <: ty1'
             then return ty2'
             else throwE $ TypeMissmatch info $ "incorrect application of abstraction"
         _ -> throwE $ TypeMissmatch info $ "incorrect application"

infer (TAbs _ name ty t) = do
    names <- lift get
    lift $ modify $ bind name (VarBind ty)
    ty' <- infer t
    lift $ put names
    return $ TyArrow ty (typeShift (-1) ty')

computeType :: a -> Type -> Maybe Type
computeType _ tyS@(TyRec _ tyS1) = Just $ typeSubstitutionTop tyS tyS1
computeType _ _ = Nothing

simplifyType :: a -> Type -> Type
simplifyType c ty = case computeType c ty of
                         Just x -> simplifyType c x
                         _ -> ty

typeMap :: (Int -> Depth -> VarName -> Type) -> Int -> Type -> Type
typeMap onVar s ty = walk s ty
               where walk c (TyVar x n) = onVar c x n
                     walk c (TyRec x ty1) = TyRec x (walk (c + 1) ty1)
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

instance Show TypeError where
    show (TypeMissmatch info message) = message ++ " in " ++ (show $ row info) ++ ":" ++ (show $ column info)