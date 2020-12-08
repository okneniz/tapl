module Language.TAPL.FullRecon.Evaluator (evalString) where

import Data.List (last)
import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileM)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.FullRecon.Types
import Language.TAPL.FullRecon.Parser
import Language.TAPL.FullRecon.Context
import Language.TAPL.FullRecon.TypeChecker
import Language.TAPL.FullRecon.Pretty

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, ns) -> runExcept (evalStateT (f commands) (newState ns))
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Command] -> Eval [String]
evalCommands [] = return []
evalCommands ((Bind _ name binding):cs) = do
   modify $ \s -> s { names = (bind name binding (names s)) }
   evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval (t:ts)):cs) = do
    ty <- typeOf t
    let t' = fromJust $ whileM normalize t
    (:) <$> render t' ty <*> evalCommands ((Eval ts):cs)

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf p t1 t2 t3) = TIf p <$> normalize t1 <*> return t2 <*> return t3
normalize (TSucc p t1) = TSucc p <$> normalize t1
normalize (TPred _ (TZero p)) = return $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t
normalize (TPred p t) = TPred p <$> normalize t
normalize (TIsZero _ (TZero p)) = return $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = return $ TFalse p
normalize (TIsZero p t) = TIsZero p <$> normalize t
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = flip(TApp p) t2 <$> normalize t1
normalize (TLet _ _ t1 t2) | isVal t1 = return $ termSubstitutionTop t1 t2
normalize (TLet p v t1 t2) = flip(TLet p v) t2 <$> normalize t1
normalize _ = Nothing
