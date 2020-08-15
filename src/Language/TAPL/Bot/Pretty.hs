module Language.TAPL.Bot.Pretty (prettify, prettifyType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (withTmpStateT)
import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context

prettify :: Term -> Eval (Doc a)
prettify (TVar _ varname _) = do
    c <- get
    case nameFromContext c varname of
         Just name -> return $ pretty name
         Nothing -> lift $ throwE $ "[bad index " ++ show varname ++ " in context " ++ show c  ++ "]"

prettify (TAbs _ name _ t) = do
    names <- get
    let newName = fst $ pickFreshName names name
    withTmpStateT (addName newName) $ do
        doc <- prettify t
        return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ doc1 <+> doc2

prettifyType :: Type -> Eval (Doc a)
prettifyType ty = return $ pretty ty

instance Pretty Type where
    pretty TyTop = pretty "Top"
    pretty TyBot = pretty "Bot"
    pretty (TyArrow ty1 ty2) = pretty ty1 <+> pretty "->" <+> pretty ty2
