module Language.TAPL.Untyped.Evaluator (evalString) where

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Parser
import Language.TAPL.Untyped.Pretty

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

evalString :: String  -> Either String String
evalString code = do
    case parse "<stdin>" code of
        Left e -> Left $ show e
        Right ([], _) -> return ""
        Right (ast, names) -> runExcept (evalStateT (f ast) names)
    where
        f ast = fmap show $ prettify $ last $ whileJust normalize <$> ast

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp p t1 t2) | isVal t1  = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = flip(TApp p) t2 <$> normalize t1
normalize _ = Nothing
