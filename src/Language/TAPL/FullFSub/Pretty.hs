module Language.TAPL.FullFSub.Pretty (render, renderType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (withTmpStateT)
import Language.TAPL.Common.Context (nameFromContext, findName)
import Language.TAPL.FullFSub.Types
import Language.TAPL.FullFSub.Context

render :: Term -> Type -> Eval String
render t ty = do
    docT <- prettify t
    docTy <- prettifyType ty
    return $ show $ docT <> colon <> docTy

renderType :: Type -> Eval String
renderType ty = show <$> prettifyType ty

prettify :: Term -> Eval (Doc a)
prettify (TTrue _) = return $ pretty "true"
prettify (TFalse _) = return $ pretty "false"
prettify (TString _ s) = return $ dquotes $ pretty s
prettify (TUnit _) = return $ pretty "unit"
prettify (TZero _) = return $ pretty "zero"
prettify (TFloat _ t) = return $ pretty t

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
         Nothing -> lift $ throwE $ "[bad index " <> show varname <> " in context " <> show c  <> "]"

prettify (TAbs _ name _ t) = do
  names <- get
  let newName = fst $ pickFreshName names name
  withTmpStateT (addName newName) $ do
      doc <- prettify t
      return $ parens
             $ pretty "lambda"
           <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ doc1 <+> doc2

prettify (TRecord _ ts) | Map.null ts = return $ pretty "{}"
prettify (TRecord _ ts) = do
    ts' <- sequence (f <$> Map.toList ts)
    return $ braces $ foldl1 (\x y -> x <> comma <+> y) ts'
    where f (s, t) = do
            doc <- prettify t
            return $ pretty s <> equals <> doc

prettify (TProj _ t k) = do
    doc1 <- prettify t
    return $ doc1 <> dot <> pretty k

prettify (TLet _ v t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ pretty "let"
         <+> align (pretty v <+> equals <+> doc1 <+> pretty "in" <+> doc2)

prettify (TAscribe _ t ty) = do
    docT <- prettify t
    docTy <- prettifyType ty
    return $ docT <> colon <> docTy

prettify (TFix _ t) = prettify t

prettify (TTAbs _ name _ t) = do
  names <- get
  let newName = fst $ pickFreshName names name
  withTmpStateT (addName newName) $ do
      doc <- prettify t
      return $ parens
             $ pretty "lambda"
           <+> pretty newName <> dot <> doc

prettify (TPack _ ty1 t ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettify t
    doc3 <- prettifyType ty2
    return $ braces $ pretty "*" <> doc1 <> comma
                  <+> doc2
                  <+> pretty "as"
                  <+> doc3

prettifyType :: Type -> Eval (Doc a)
prettifyType TyTop = return $ pretty "Top"
prettifyType TyBool = return $ pretty "Bool"
prettifyType TyString = return $ pretty "String"
prettifyType TyUnit = return $ pretty "Unit"
prettifyType TyNat = return $ pretty "Nat"
prettifyType TyFloat = return $ pretty "Float"
prettifyType (TyID s) = return $ pretty s

prettifyType (TyArrow ty1 ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettifyType ty2
    return $ parens (doc1 <+> pretty "->" <+> doc2)

prettifyType (TyRecord ts) = do
    ts' <- sequence (f <$> Map.toList ts)
    return $ braces $ foldl1 (\x y -> x <> comma <+> y) ts'
    where f (k, ty) = do
            doc <- prettifyType ty
            return $ pretty k <> equals <> doc

prettifyType (TyVar varName ty) = do
    names <- get
    case findName names varName of
         Just x -> return $ pretty x
         Nothing -> lift $ throwE $ show $ pretty "[wtf?] "
                                       <+> pretty varName
                                       <+> pretty "in"
                                       <+> (pretty $ show names)
                                       <+> pretty "?"

prettifyType (TySome x ty1 ty2) = do
    names <- get
    let newName = fst $ pickFreshName names x
    withTmpStateT (addName newName) $ do
        doc1 <- prettifyType ty1
        doc2 <- prettifyType ty2
        return $ braces $ pretty "Some" <+> pretty newName <> pretty "<:" <> doc1 <> dot <+> doc2

prettifyType (TyAll x ty1 ty2) = do
    names <- get
    let newName = fst $ pickFreshName names x
    withTmpStateT (addName newName) $ do
        doc1 <- prettifyType ty1
        doc2 <- prettifyType ty2
        return $ parens $ pretty "All" <+> pretty newName <> pretty "<:" <> doc1 <> dot <+> doc2
