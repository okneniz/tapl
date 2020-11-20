module Language.TAPL.Equirec.Evaluator (evalString) where

import Data.List (last)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Parser
import Language.TAPL.Equirec.Context
import Language.TAPL.Equirec.TypeChecker
import Language.TAPL.Equirec.Pretty

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, names) -> runExcept $ evalStateT (f commands) names
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Command] -> Eval [String]
evalCommands [] = return []

evalCommands ((Bind _ name b):cs) = do
    modify $ bind name b
    evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval (t:ts)):cs) = do
    ty <- typeOf t
    let t' = whileJust normalize t
    (:) <$> render t' ty <*> evalCommands ((Eval ts):cs)

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = flip(TApp p) t2 <$> normalize t1
normalize _ = Nothing
