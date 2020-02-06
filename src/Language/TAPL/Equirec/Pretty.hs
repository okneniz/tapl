module Language.TAPL.Equirec.Pretty (render, renderType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Context

type Printed a = ExceptT String (State LCNames) a

render :: LCNames -> Term -> Either String String
render names term =
    case evalState (runExceptT (prettify term)) names of
         Left x -> Left x
         Right x -> return $ show x

prettify :: Term -> Printed (Doc a)
prettify (TVar _ varname _) = do
    names <- lift $ get
    case nameFromContext names varname of
         Just name -> return $ pretty name
         Nothing -> throwE $ "[bad index " ++ show varname ++ " in context]"

prettify (TAbs _ name _ t) = do
    names <- lift $ get
    let (newName, names') = pickFreshName names name
    lift $ put names'
    doc <- prettify t
    lift $ put names
    return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ doc1 <+> doc2

renderType :: LCNames -> Type -> Either String String
renderType names ty =
    case evalState (runExceptT (prettifyType ty)) names of
         Left x -> Left x
         Right x -> return $ show x

prettifyType :: Type -> Printed (Doc a)
prettifyType (TyID x) = return $ pretty x

prettifyType (TyArrow ty1 ty2) = do
    ty1' <- prettifyType ty1
    ty2' <- prettifyType ty2
    return $ (pretty $ show ty1') <+> pretty "->" <+> (pretty $ show ty2')

prettifyType (TyRec x ty) = do
    ty' <- prettifyType ty
    return $ pretty "Rec" <+> pretty x <> dot <> ty'

prettifyType (TyVar name _) = do
    names <- lift $ get
    case nameFromContext names name of
         Just s -> return $ pretty s
         Nothing -> throwE $ "[bad index in " ++ show name ++ "]"

