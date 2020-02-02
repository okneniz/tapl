module Language.TAPL.FullSimple.Pretty (render, pretty) where

import Prelude hiding ((<>))
import Data.Text.Prettyprint.Doc

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except

import qualified Data.Map.Strict as Map

import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Context

type Printed a = ExceptT String (State LCNames) a

render :: LCNames -> Term -> Either String String
render names term =
    case evalState (runExceptT (prettify term)) names of
         Left x -> Left x
         Right x -> return $ show x

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

prettify (TVar _ varname depth) = do
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

prettify (TLookup _ t k) = do
    doc1 <- prettify t
    doc2 <- prettify k
    return $ doc1 <> dot <> doc2

prettify (TLet _ v t1 t2) = do
    doc1 <- prettify t1
    doc2 <- prettify t2
    return $ pretty "let" <+> align (pretty v <+> equals <+> doc1 <+> pretty "in" <+> doc2)

prettify (TTag _ key t ty) = do
    doc <- prettify t
    return $ angles $ pretty key <> equals <> doc

prettify (TAscribe _ t ty) = do
    doc <- prettify t
    return $ doc <> colon <> pretty ty

prettify (TCase _ t cs) = do
    doc <- prettify t
    cases <- sequence $ renderCase <$> Map.toList cs
    let cases' = foldl1 (\x y -> x <> hardline <> y) cases
    return $ pretty "case" <> align (doc <+> pretty "of" <+> hardline <> cases')
    where renderCase (caseName, (varName, t)) = do
            docC <- prettify t
            return $ pretty "|" <+> (angles (pretty caseName <> equals <> pretty varName) <+> pretty "->" <+> docC)

prettify (TFix _ t) = prettify t

instance Pretty Type where
    pretty TyBool = pretty "Bool"
    pretty TyInt = pretty "Int"
    pretty TyString = pretty "String"
    pretty TyUnit = pretty "Unit"
    pretty TyNat = pretty "Nat"
    pretty TyFloat = pretty "Float"
    pretty TyKeyword = pretty "Keyword"
    pretty (TyID s) = pretty s
    pretty (TyArrow ty1 ty2) = parens (pretty ty1 <+> pretty "->" <+> pretty ty2)
    pretty (TyProduct ty1 ty2) = braces (pretty ty1 <> pretty "*" <> pretty ty2)

    pretty (TyRecord ts) = braces $ foldl1 (\x y -> x <> comma <+> y) $ field <$> Map.toList ts
      where field (k,t) = pretty k <> equals <> pretty t

    pretty (TyVariant ts) = angles $ foldl1 (\x y -> x <> comma <+> y) $ field <$> Map.toList ts
      where field (k,t) = pretty k <> colon <> pretty t