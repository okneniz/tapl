module Language.TAPL.Bot.Pretty (render, renderType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (withTmpStateT)
import Language.TAPL.Common.Context (nameFromContext)
import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context

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
    let newName = fst $ pickFreshName names name
    withTmpStateT (addName newName) $ do
        doc <- prettify t
        return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = (<+>) <$> prettify t1 <*> prettify t2

prettifyType :: Type -> Eval (Doc a)
prettifyType ty = return $ pretty ty

instance Pretty Type where
    pretty TyTop = pretty "Top"
    pretty TyBot = pretty "Bot"
    pretty (TyArrow ty1 ty2) = pretty ty1 <+> pretty "->" <+> pretty ty2
