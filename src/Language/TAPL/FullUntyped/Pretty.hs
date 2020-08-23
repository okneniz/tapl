module Language.TAPL.FullUntyped.Pretty (prettify) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.FullUntyped.Types
import Language.TAPL.FullUntyped.Context
import Language.TAPL.Common.Context (nameFromContext)

prettify :: Term -> Eval (Doc a)
prettify (TTrue _) = return $ pretty "true"
prettify (TFalse _) = return $ pretty "false"
prettify (TString _ s) = return $ dquotes $ pretty s
prettify (TUnit _) = return $ pretty "unit"
prettify (TZero _) = return $ pretty "zero"
prettify (TFloat _ t) = return $ pretty t
prettify (TInt _ x) = return $ pretty x
prettify (TKeyword _ x) = return $ pretty x

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
    c <- get
    case nameFromContext c varname of
         Just name -> return $ pretty name
         Nothing -> lift $ throwE $ "[bad index " ++ show varname ++ " in context " ++ show c  ++ "]"

prettify (TAbs _ name t) = do
  names <- get
  let (newName, names') = pickFreshName names name
  put names'
  doc <- prettify t
  put names
  return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = (<+>) <$> prettify t1 <*> prettify t2

prettify (TPair _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ braces (doc1 <> comma <> doc2)

prettify (TRecord _ ts) = do
    ts' <- sequence $ (f <$> Map.toList ts)
    return $ braces $ foldl1 (\x y -> x <> comma <+> y) ts'
    where f (s, t) = do
            doc <- prettify t
            return $ pretty s <> equals <> doc

prettify (TProj _ t k) = do
    doc1 <- prettify t
    doc2 <- prettify k
    return $ doc1 <> dot <> doc2

prettify (TLet _ v t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ pretty "let" <+> align (pretty v <+> equals <+> doc1 <+> pretty "in" <+> doc2)

prettify (TTimesFloat _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ pretty "timesfloat" <+> doc1 <+> doc2