module Language.TAPL.Untyped.Pretty (render) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Context

type Printed a = ExceptT String (State LCNames) a

render :: LCNames -> Term -> Either String String
render names term =
    case evalState (runExceptT (prettify term)) names of
         Left x -> Left x
         Right x -> return $ show x

prettify :: Term -> Printed (Doc a)
prettify (TVar _ varname _) = do
    c <- lift $ get
    case nameFromContext c varname of
         Just name -> return $ pretty name
         Nothing -> throwE $ "[bad index " ++ show varname ++ " in context " ++ show c  ++ "]"

prettify (TAbs _ name t) = do
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

