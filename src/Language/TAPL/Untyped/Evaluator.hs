module Language.TAPL.Untyped.Evaluator (evalString) where

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Parser
import Language.TAPL.Untyped.Pretty

evalString :: String  -> Either String String
evalString code = do
  case parse "<stdint>" code of
    Left e -> Left $ show e
    Right (ast, names) -> do
        let result = last $ whileJust normalize <$> ast
        result' <- render names result
        return result'

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp p t1 t2) | isVal t1  = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = normalize t1 >>= \t1' -> return $ TApp p t1' t2
normalize _ = Nothing
