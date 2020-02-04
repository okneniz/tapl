module Language.TAPL.TypedArith.Pretty (render, pretty) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.TAPL.TypedArith.Types
import Language.TAPL.TypedArith.Context

type Printed a = ExceptT String (State LCNames) a

render :: LCNames -> Term -> Either String String
render names term =
    case evalState (runExceptT (prettify term)) names of
         Left x -> Left x
         Right x -> return $ show x

prettify :: Term -> Printed (Doc a)
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
    c <- lift $ get
    case nameFromContext c varname of
         Just name -> return $ pretty name
         Nothing -> throwE $ "[bad index " ++ show varname ++ " in context " ++ show c  ++ "]"

prettify (TAbs _ name _ t) = do
  names <- lift $ get
  let (newName, names') = pickFreshName names name
  lift $ put names'
  doc <- prettify t
  lift $ put names
  return $ parens $ pretty "lambda" <+> pretty newName <> dot <> doc

prettify (TApp _ t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ doc1 <+> doc2

instance Pretty Type where
    pretty TyBool = pretty "Bool"
    pretty TyNat = pretty "Nat"
    pretty (TyArrow ty1 ty2) = parens (pretty ty1 <+> pretty "->" <+> pretty ty2)
