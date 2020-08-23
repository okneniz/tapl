module Language.TAPL.FullError.Pretty (prettify, prettifyType) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Common.Helpers (withTmpStateT)
import Language.TAPL.Common.Context (nameFromContext, findName)
import Language.TAPL.FullError.Types
import Language.TAPL.FullError.Context

prettify :: Term -> Eval (Doc a)
prettify (TTrue _) = return $ pretty "true"
prettify (TFalse _) = return $ pretty "false"

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

prettify (TError _) = return $ pretty "error"

prettify (TTry _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ align
           $ fillSep [ (pretty "try" <+> doc1)
                     , (pretty "with" <+> doc2)
                     ]


prettifyType :: Type -> Eval (Doc a)
prettifyType TyTop = return $ pretty "Top"
prettifyType TyBot = return $ pretty "Bot"
prettifyType TyBool = return $ pretty "Bool"

prettifyType (TyArrow ty1 ty2) = do
    doc1 <- prettifyType ty1
    doc2 <- prettifyType ty2
    return $ doc1 <+> pretty "->" <+> doc2

prettifyType (TyVar varName ty) = do
    names <- get
    case findName names varName of
         Just x -> return $ pretty x
         Nothing -> lift $ throwE $ show $ pretty "[wtf?] "
                                       <+> pretty varName
                                       <+> pretty "in"
                                       <+> (pretty $ show names)
                                       <+> pretty "?"
