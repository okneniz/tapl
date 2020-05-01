module Language.TAPL.FullRecon.Pretty (render, pretty) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Language.TAPL.FullRecon.Types
import Language.TAPL.FullRecon.Context

render :: Term -> Eval String
render t = do
    x <- prettify t
    return $ show x

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
    names <- ask
    case nameFromContext names varname of
         Just name -> return $ pretty name
         Nothing -> lift $ throwE $ "[bad index " ++ show varname ++ "]"

prettify (TAbs _ name _ t) = do
    names <- ask
    let (newName, names') = pickFreshName names name
    local (const names') $ do
        doc <- prettify t
        return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ doc1 <+> doc2

prettify (TLet _ x t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ pretty "let "
         <+> pretty x
         <+> pretty " = "
         <+> doc1
         <+> pretty " in "
         <+> doc2

instance Pretty Type where
    pretty TyBool = pretty "Bool"
    pretty TyNat = pretty "Nat"
    pretty (TyID s) = pretty s
    pretty (TyArrow ty1 ty2) = parens $ pretty ty1 <+> pretty "->" <+> pretty ty2
