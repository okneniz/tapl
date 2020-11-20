module Language.TAPL.PureFSub.Pretty (render, renderType, prettifyType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.PureFSub.Types
import Language.TAPL.PureFSub.Context
import Language.TAPL.Common.Helpers (withTmpStateT)
import Language.TAPL.Common.Context (nameFromContext)

render :: Term -> Type -> Eval String
render t ty = do
    docT <- prettify t
    docTy <- prettifyType ty
    return $ show $ docT <> colon <> docTy

renderType :: Type -> Eval String
renderType ty = show <$> prettifyType ty

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

prettify (TTAbs _ name _ t) = do
  names <- get
  let newName = fst $ pickFreshName names name
  withTmpStateT (addName newName) $ do
      doc <- prettify t
      return $ parens
             $ pretty "lambda"
           <+> pretty newName <> dot <> doc

prettifyType :: Type -> Eval (Doc a)
prettifyType TyTop = return $ pretty "Top"

prettifyType (TyArrow ty1 ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettifyType ty2
    return $ parens (doc1 <+> pretty "->" <+> doc2)

prettifyType (TyVar name _) = do
    names <- get
    case nameFromContext names name of
         Just s -> return $ pretty s
         Nothing -> lift $ throwE $ "[bad index in " <> show name <> "]"

prettifyType (TyAll x ty1 ty2) = do
    names <- get
    let newName = fst $ pickFreshName names x
    withTmpStateT (addName newName) $ do
        doc1 <- prettifyType ty1
        doc2 <- prettifyType ty2
        return $ parens $ pretty "All" <+> pretty newName <> pretty ":" <> doc1 <> pretty "<:" <> doc2
