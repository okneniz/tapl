module Language.TAPL.SimpleBool.Evaluator (evalString) where


import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.SimpleBool.Context
import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Parser
import Language.TAPL.SimpleBool.TypeChecker
import Language.TAPL.SimpleBool.Pretty

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, names) -> runExcept $ evalStateT (f commands) names
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Term] -> Eval [String]
evalCommands [] = return []
evalCommands (t:ts) = do
    ty <- typeOf t
    let t' = whileJust normalize t
    (:) <$> render t' ty <*> evalCommands ts

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _ ) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf p t1 t2 t3) = TIf p <$> normalize t1 <*> return t2 <*> return t3
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp pos t1 t2) | isVal t1 = TApp pos t1 <$> normalize t2
normalize (TApp p t1 t2) = flip(TApp p) t2 <$> normalize t1
normalize _ =  Nothing
