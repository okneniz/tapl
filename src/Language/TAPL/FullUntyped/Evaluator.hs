module Language.TAPL.FullUntyped.Evaluator (evalString) where

import qualified Data.Map.Lazy as Map

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.FullUntyped.Types
import Language.TAPL.FullUntyped.Parser
import Language.TAPL.FullUntyped.Pretty
import Language.TAPL.FullUntyped.Context

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
evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval (t:ts)):cs) = do
    let t' = whileJust normalize t
    (:) <$> render t' <*> evalCommands ((Eval ts):cs)

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _ ) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf p t1 t2 t3) = TIf p <$> normalize t1 <*> return t2 <*> return t3

normalize (TApp _ (TAbs _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = flip(TApp p) t2 <$> normalize t1

normalize (TSucc p t) = TSucc p <$> normalize t

normalize (TPred _ (TZero p)) = return $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred p t) = TPred p <$> normalize t

normalize (TIsZero _ (TZero p)) = return $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = return $ TFalse p
normalize (TIsZero p t) = TIsZero p <$> normalize t

normalize (TPair p t1 t2) | isVal t1 = TPair p t1 <$> normalize t2
normalize (TPair p t1 t2) = flip(TPair p) t2 <$> normalize t1

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing
normalize (TRecord p fs) = do
    fs' <- sequence $ evalField <$> Map.toList fs
    return $ TRecord p (Map.fromList fs')
    where evalField (k, v) | isVal v = return (k, v)
          evalField (k, v) = ((,) k) <$> normalize v

normalize (TProj _ t@(TRecord _ fs) k) | isVal t = Map.lookup k fs
normalize (TProj p t@(TRecord _ _) k) = flip(TProj p) k <$> normalize t

normalize (TProj _ (TPair _ t _) "0") | isVal t = return t
normalize (TProj _ (TPair _ _ t) "1") | isVal t = return t
normalize (TProj p t k) = flip(TProj p) k <$> normalize t

normalize (TLet _ _ t1 t2) | isVal t1 = return $ substitutionTop t1 t2
normalize (TLet p v t1 t2) = flip(TLet p v) t2 <$> normalize t1

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) = return $ TFloat p (t1 * t2)
normalize (TTimesFloat p t1 t2) | isVal t1 = TTimesFloat p t1 <$> normalize t2
normalize (TTimesFloat p t1 t2) = flip(TTimesFloat p) t2 <$> normalize t1

normalize _ = Nothing
