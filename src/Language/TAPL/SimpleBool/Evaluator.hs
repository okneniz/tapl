module Language.TAPL.SimpleBool.Evaluator (evalString) where

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Parser
import Language.TAPL.SimpleBool.TypeChecker
import Language.TAPL.SimpleBool.Pretty

import Data.List (last)

evalString :: String -> Either String String
evalString code = do
  case parse "<stdin>" code of
    Left e -> Left $ show e
    Right (ast, names) -> do
      _ <- sequence $ typeOf names <$> ast
      let result = last $ eval ast
      ty <- typeOf names result
      result' <- render names result
      return $ result' ++ ":" ++ (show $ pretty ty)

eval :: AST -> AST
eval ast = whileJust normalize <$> ast

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _ ) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf pos t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf pos t1' t2 t3
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp pos t1 t2) | isVal t1 = TApp pos t1 <$> normalize t2
normalize (TApp pos t1 t2) = normalize t1 >>= \t1' -> return $ TApp pos t1' t2
normalize _ =  Nothing
