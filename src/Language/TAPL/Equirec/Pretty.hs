module Language.TAPL.Equirec.Pretty (prettify, prettifyType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (withTmpStateT)
import Language.TAPL.Common.Context (nameFromContext, findName)
import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Context

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
      return $ parens
             $ pretty "lambda"
           <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ doc1 <+> doc2

prettifyType :: Type -> Eval (Doc a)
prettifyType (TyID s) = return $ pretty s

prettifyType (TyArrow ty1 ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettifyType ty2
    return $ parens (doc1 <+> pretty "->" <+> doc2)

prettifyType (TyRec name ty) = do
    names <- get
    let newName = fst $ pickFreshName names name
    withTmpStateT (addName newName) $ do
        doc <- prettifyType ty
        return $ pretty "Rec"
             <+> pretty newName
             <> pretty "."
             <> doc

prettifyType (TyVar varName ty) = do
    names <- get
    case findName names varName of
         Just x -> return $ pretty x
         Nothing -> lift $ throwE $ show $ pretty "[wtf?] "
                                       <+> pretty varName
                                       <+> pretty "in"
                                       <+> (pretty $ show names)
                                       <+> pretty "?"
