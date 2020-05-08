module Language.TAPL.Untyped.Evaluator (evalString) where

import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Parser
import Language.TAPL.Untyped.Pretty

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
        let result = last $ fullNormalize <$> ast
        result' <- render names result
        return result'

fullNormalize :: Term -> Term
fullNormalize t = case normalize t of
                       Just t' -> fullNormalize t'
                       Nothing -> t

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp info t1 t2) | isVal t1  = TApp info t1 <$> normalize t2
normalize (TApp info t1 t2) = normalize t1 >>= \t1' -> return $ TApp info t1' t2
normalize _ = Nothing
