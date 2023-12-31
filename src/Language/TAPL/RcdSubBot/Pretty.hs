module Language.TAPL.RcdSubBot.Pretty (render, prettify, prettifyType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import qualified Data.Map.Lazy as Map

import Language.TAPL.RcdSubBot.Types
import Language.TAPL.RcdSubBot.Context
import Language.TAPL.Common.Context (nameFromContext)

render :: Term -> Type -> Eval String
render t ty = do
    docT <- prettify t
    docTy <- prettifyType ty
    return $ show $ docT <> colon <> docTy

prettify :: Term -> Eval (Doc a)
prettify (TVar _ varname _) = do
    c <- get
    case nameFromContext c varname of
         Just name -> return $ pretty name
         Nothing -> lift $ throwE $ "[bad index " <> show varname <> " in context " <> show c  <> "]"

prettify (TAbs _ name _ t) = do
  names <- get
  let (newName, names') = pickFreshName names name
  put names'
  doc <- prettify t
  put names
  return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = (<+>) <$> prettify t1 <*> prettify t2

prettify (TRecord _ ts) | Map.null ts = return $ pretty "{}"
prettify (TRecord _ ts) = do
    ts' <- sequence (f <$> Map.toList ts)
    return $ braces $ foldl1 (\x y -> x <> comma <> y) ts'
    where f (s, t) = do
            doc <- prettify t
            return $ pretty s <> equals <> doc

prettify (TProj _ t k) = do
    doc1 <- prettify t
    return $ doc1 <> dot <> pretty k

prettifyType :: Type -> Eval (Doc a)
prettifyType TyTop = return $ pretty "Top"
prettifyType TyBot = return $ pretty "Bot"

prettifyType (TyArrow ty1 ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettifyType ty2
    return $ doc1 <+> pretty "->" <+> doc2

prettifyType (TyRecord ts) | Map.null ts = return $ pretty "{}"
prettifyType (TyRecord ts) = do
    ts' <- sequence (f <$> Map.toList ts)
    return $ braces $ foldl1 (\x y -> x <> comma <> y) ts'
    where f (k, ty) = do
            doc <- prettifyType ty
            return $ pretty k <> colon <> doc

