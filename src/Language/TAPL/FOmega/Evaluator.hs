module Language.TAPL.FOmega.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad (liftM, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Text.Parsec (SourcePos)

import Language.TAPL.FOmega.Types
import Language.TAPL.FOmega.Parser
import Language.TAPL.FOmega.Context
import Language.TAPL.FOmega.TypeChecker
import Language.TAPL.FOmega.Pretty
import Language.TAPL.Common.Context (bind)

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

continue :: (Monad m1, Monad m2) => a -> m1 (m2 a)
continue x = (return.return) x

nvm :: Eval (Maybe Term)
nvm = return Nothing

normalize :: Term -> Eval (Maybe Term)
normalize (TApp p (TAbs _ _ tyT11 t12) v2) | isVal v2 = continue $ termSubstitutionTop v2 t12
normalize (TApp p v1 t2) | isVal v1 = liftM(TApp p v1) <$> normalize t2
normalize (TApp p t1 t2) = liftM(flip (TApp p) t2) <$> normalize t1

normalize (TTApp p (TTAbs _ _ _ t11) tyT2) = continue $ typeTermSubstitutionTop tyT2 t11
normalize (TTApp p t1 tyT2) = liftM(flip (TTApp p) tyT2) <$> normalize t1
normalize _ = nvm
