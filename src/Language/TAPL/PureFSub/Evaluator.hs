module Language.TAPL.PureFSub.Evaluator (evalString) where

import qualified Data.Map.Lazy as Map

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.PureFSub.Types
import Language.TAPL.PureFSub.Parser
import Language.TAPL.PureFSub.TypeChecker
import Language.TAPL.PureFSub.Pretty
import Language.TAPL.PureFSub.Context

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

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
normalize (TTApp _ (TTAbs _ _ _ t11) tyT2) = return $ typeTermSubstitutionTop tyT2 t11
normalize (TTApp p t1 tyT2) = flip (TTApp p) tyT2 <$> normalize t1
normalize (TApp _ (TAbs _ _ tyT11 t12) v2) | isVal v2 = return $ termSubstitutionTop v2 t12
normalize (TApp p v1 t2) | isVal v1 = TApp p v1 <$> normalize t2
normalize (TApp p t1 t2) = flip (TApp p) t2 <$> normalize t1
normalize _ = Nothing
