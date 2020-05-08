module Language.TAPL.TypedArith.Evaluator (evalString) where

import Language.TAPL.TypedArith.Types
import Language.TAPL.TypedArith.Parser
import Language.TAPL.TypedArith.TypeChecker
import Language.TAPL.TypedArith.Pretty

import Data.List (last)

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
      _ <- sequence $ typeOf names <$> ast
      let result = last $ eval ast
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
normalize (TIf _ (TTrue _) t _ ) = return t
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
normalize _ = Nothing

