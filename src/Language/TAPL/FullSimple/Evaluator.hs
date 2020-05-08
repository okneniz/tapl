module Language.TAPL.FullSimple.Evaluator (evalString) where

import Data.List (all, last)
import qualified Data.Map.Strict as Map

import Control.Monad (liftM)

import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Parser
import Language.TAPL.FullSimple.TypeChecker
import Language.TAPL.FullSimple.Pretty

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
      _ <- sequence $ typeOf names <$> ast
      let result = last  $ eval ast
      ty <- typeOf names result
      result' <- render names result
      return $ result' ++ ":" ++ (show $ pretty ty)

eval :: AST -> AST
eval ast = fullNormalize <$> ast

fullNormalize :: Term -> Term
fullNormalize t = case normalize t of
                       Just t' -> fullNormalize t'
                       Nothing -> t

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf info t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf info t1' t2 t3

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp info t1 t2) | isVal t1 = TApp info t1 <$> normalize t2
normalize (TApp info t1 t2) = normalize t1 >>= \t1' -> return $ TApp info t1' t2

normalize (TSucc info t) = TSucc info <$> normalize t

normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred info t) = TPred info <$> normalize t

normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t = return $ TFalse info
normalize (TIsZero info t) = TIsZero info <$> normalize t

normalize (TPair info t1 t2) | isVal t1 = TPair info t1 <$> normalize t2
normalize (TPair info t1 t2) = normalize t1 >>= \t1' -> return $ TPair info t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord info fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields
    return $ TRecord info (Map.fromList fields')
    where evalField (k, field) | isVal field = return (k, field)
          evalField (k, field) = do
            field' <- normalize field
            return (k, field')

normalize (TLookup _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = Map.lookup key fields
normalize (TLookup info t@(TRecord _ _) (TKeyword x key)) = do
    t' <- normalize t
    return $ (TLookup info t' (TKeyword x key))

normalize (TLookup _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TLookup _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t

normalize (TLookup info t k) = normalize t >>= \t' -> return $ TLookup info t' k

normalize (TLet _ _ t1 t2) | isVal t1 = return $ substitutionTop t1 t2
normalize (TLet info v t1 t2) = normalize t1 >>= \t1' -> return $ TLet info v t1' t2

normalize (TAscribe _ t _) = return t

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = return $ substitutionTop t1 t2
normalize (TFix info t) = TFix info <$> normalize t

normalize (TCase _ (TTag _ key v _) branches) | isVal v =
    liftM (\(_, t) -> substitutionTop v t)
          (Map.lookup key branches)

normalize (TCase info t fields) = normalize t >>= \t' -> return $ TCase info t' fields
normalize _ = Nothing
