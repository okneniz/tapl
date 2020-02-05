module Language.TAPL.Bot.Pretty (render, pretty) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Context

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
    pretty TyTop = pretty "Top"
    pretty TyBot = pretty "Bot"
    pretty (TyArrow ty1 ty2) = pretty ty1 <+> pretty "->" <+> pretty ty2
