module Language.TAPL.Fomega.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Text.Parsec (SourcePos)

import Language.TAPL.Fomega.Types
import Language.TAPL.Fomega.Parser
import Language.TAPL.Fomega.Context
import Language.TAPL.Fomega.TypeChecker
import Language.TAPL.Fomega.Pretty
import Language.TAPL.Common.Context (bind)
import Language.TAPL.Common.Helpers (ok, nvm)

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, names) -> runExcept $ evalStateT (f commands) []
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Command] -> Eval [String]
evalCommands [] = return []

evalCommands ((Bind p name b):cs) = do
    modify $ bind name b
    evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval (t:ts)):cs) = do
    ty <- typeOf t
    t' <- fullNormalize t
    (:) <$> render t' ty <*> evalCommands ((Eval ts):cs)

fullNormalize :: Term -> Eval Term
fullNormalize t = do
    t' <- normalize t
    case t' of
         Just x -> fullNormalize x
         _ -> return t

normalize :: Term -> Eval (Maybe Term)
normalize (TApp p (TAbs _ _ tyT11 t12) v2) | isVal v2 = ok $ termSubstitutionTop v2 t12
normalize (TApp p v1 t2) | isVal v1 = fmap(TApp p v1) <$> normalize t2
normalize (TApp p t1 t2) = fmap(flip (TApp p) t2) <$> normalize t1

normalize (TTApp p (TTAbs _ _ _ t11) tyT2) = ok $ typeTermSubstitutionTop tyT2 t11
normalize (TTApp p t1 tyT2) = fmap(flip (TTApp p) tyT2) <$> normalize t1
normalize _ = nvm
