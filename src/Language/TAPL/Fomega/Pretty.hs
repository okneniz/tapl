module Language.TAPL.Fomega.Pretty (render, renderType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (withTmpStateT)
import Language.TAPL.Common.Context (nameFromContext, findName)
import Language.TAPL.Fomega.Types
import Language.TAPL.Fomega.Context

render :: Term -> Type -> Eval String
render t ty = do
    docT <- prettify t
    docTy <- prettifyType ty
    return $ show $ docT <> colon <> docTy

renderType :: Type -> Eval String
renderType ty = show <$> prettifyType ty

prettyConst s = return $ pretty s
prettyFunc n t = (pretty n <+>) <$> prettify t
prettyTypeFunc n t = (pretty n <+>) <$> prettifyType t

prettify :: Term -> Eval (Doc a)
prettify (TVar _ varname _) = do
    c <- get
    case nameFromContext c varname of
         Just name -> return $ pretty name
         Nothing -> lift $ throwE $ "[bad index " <> show varname <> " in context " <> show c <> "]"

prettify (TAbs _ name _ t) = do
  n <- get
  let newName = fst $ pickFreshName n name
  withTmpStateT (addName newName) $ do
      doc <- prettify t
      return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = (<+>) <$> prettify t1 <*> prettify t2

prettify (TTAbs _ name _ t) = do
  n <- get
  let newName = fst $ pickFreshName n name
  withTmpStateT (addName newName) $ do
      doc <- prettify t
      return $ parens
             $ pretty "lambda"
           <+> pretty newName <> dot <> doc

prettify (TTApp _ t ty) = (<+>) <$> prettify t <*> prettifyType ty

prettifyType :: Type -> Eval (Doc a)
prettifyType (TyArrow ty1 ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettifyType ty2
    return $ parens (doc1 <+> pretty "->" <+> doc2)

prettifyType v@(TyVar varName _) = do
    names <- get
    case findName names varName of
         Just x -> return $ pretty x
         Nothing -> return $ pretty $ show v <> " in " <> show names

prettifyType (TyAll x k ty) = do
     n <- get
     let newName = fst $ pickFreshName n x
     withTmpStateT (addName newName) $ do
         doc1 <- prettifyKind k
         doc2 <- prettifyType ty
         return $ parens $ pretty "All" <+> pretty newName <> pretty "::" <> doc1 <> dot <+> doc2

prettifyType (TyApp ty1 ty2) = (<+>) <$> prettifyType ty1 <*> prettifyType ty2

prettifyKind :: Kind -> Eval (Doc a)
prettifyKind Star = return $ pretty "*"
prettifyKind (Arrow k1 k2) = do
    doc1 <- prettifyKind k1
    doc2 <- prettifyKind k2
    return $ doc1 <+> pretty "->" <+> doc2
