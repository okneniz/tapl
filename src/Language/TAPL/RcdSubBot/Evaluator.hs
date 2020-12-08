module Language.TAPL.RcdSubBot.Evaluator (evalString) where

import Data.List (last)
import Data.Maybe (fromJust)
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileM)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.RcdSubBot.Types
import Language.TAPL.RcdSubBot.Parser
import Language.TAPL.RcdSubBot.Context
import Language.TAPL.RcdSubBot.TypeChecker
import Language.TAPL.RcdSubBot.Pretty

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, names) -> runExcept $ evalStateT (f commands) names
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Command] -> Eval [String]
evalCommands [] = return []
evalCommands ((Bind _ name binding):cs) = do
   modify $ bind name binding
   evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval (t:ts)):cs) = do
    ty <- typeOf t
    let t' = fromJust $ whileM normalize t
    (:) <$> render t' ty <*> evalCommands ((Eval ts):cs)

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp pos t1 t2) | isVal t1 = TApp pos t1 <$> normalize t2
normalize (TApp p t1 t2) = flip(TApp p) t2 <$> normalize t1

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord pos fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields -- не то!
    return $ TRecord pos (Map.fromList fields')
    where evalField (k, field) | isVal field = return (k, field)
          evalField (k, field) = (,) k <$> normalize field

normalize (TProj _ t@(TRecord _ fields) key) | isVal t = Map.lookup key fields
normalize (TProj pos t@(TRecord _ _) key) = do
    t' <- normalize t
    return $ TProj pos t' key

normalize _ = Nothing
