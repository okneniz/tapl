module Language.TAPL.Recon.Pretty (render, prettify, prettifyType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers
import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Context
import Language.TAPL.Recon.TypeChecker (applySubst)
import Language.TAPL.Common.Context (nameFromContext)

render :: Term -> Type -> Eval String
render t ty = do
    docT <- prettify t
    docTy <- prettifyType =<< applySubst ty
    return $ show $ docT <> colon <> docTy

prettify :: Term -> Eval (Doc a)
prettify (TTrue _) = return $ pretty "true"
prettify (TFalse _) = return $ pretty "false"
prettify (TZero _) = return $ pretty "zero"

prettify (TSucc _ t) = do
    doc <- prettify t
    return $ pretty "succ" <+> doc

prettify (TPred _ t) = do
    doc <- prettify t
    return $ pretty "pred" <+> doc

prettify (TIsZero _ t) = do
    doc <- prettify t
    return $ pretty "zero?" <+> doc

prettify (TIf _ t1 t2 t3) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    doc3 <- prettify t3
    return $ align
           $ fillSep [ (pretty "if" <+> doc1)
                     , (pretty "then" <+> doc2)
                     , (pretty "else" <+> doc3)
                     ]

prettify (TVar _ varname _) = do
    n <- getNames
    case nameFromContext n varname of
         Just name -> return $ pretty name
         Nothing -> lift $ throwE $ "[bad index " <> show varname <> "]"

prettify (TAbs _ name _ t) = do
    state <- get
    let (newName, names') = pickFreshName (names state) name
    withTmpStateT (\s -> s { names = names' }) $ do
        doc <- prettify t
        return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = (<+>) <$> prettify t1 <*> prettify t2

prettifyType :: Type -> Eval (Doc a)
prettifyType TyBool = return $ pretty "Bool"
prettifyType TyNat = return $ pretty "Nat"
prettifyType (TyID s) = return $ pretty s
prettifyType (TyArrow ty1 ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettifyType ty2
    return $ parens $ doc1 <+> pretty "->" <+> doc2
