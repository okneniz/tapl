module Language.TAPL.FullFSubRef.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

import Language.TAPL.FullFSubRef.Types
import Language.TAPL.FullFSubRef.Parser
import Language.TAPL.FullFSubRef.Context
import Language.TAPL.FullFSubRef.TypeChecker
import Language.TAPL.FullFSubRef.Pretty
import Language.TAPL.Common.Context (bind)
import Language.TAPL.Common.Helpers (ok, nvm)

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, names) -> runExcept $ evalStateT (f commands) (emptyState names)
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Command] -> Eval [String]
evalCommands [] = return []

evalCommands ((Bind _ name b):cs) = do
    modifyNames $ bind name b
    shiftStore 1
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
normalize (TIf _ (TTrue _) t _ ) = ok t
normalize (TIf _ (TFalse _) _ t) = ok t
normalize (TIf p t1 t2 t3) = fmap(\t1' -> TIf p t1' t2 t3) <$> normalize t1

normalize (TLet _ _ t1 t2) | isVal t1 = ok $ termSubstitutionTop t1 t2
normalize (TLet p v t1 t2) = fmap(flip(TLet p v) t2) <$> normalize t1

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = ok $ termSubstitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = fmap(TApp p t1) <$> normalize t2
normalize (TApp p t1 t2) = fmap(flip(TApp p) t2) <$> normalize t1

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = ok $ termSubstitutionTop t1 t2
normalize (TFix p t) = fmap(TFix p) <$> normalize t

normalize (TRecord _ fields) | (Map.size fields) == 0 = nvm
normalize t@(TRecord _ _) | isVal t = nvm

normalize (TRecord p fs) = do
    fs' <- mapM evalField $ Map.toList fs
    ok $ TRecord p (Map.fromList fs')
    where evalField (k,v) = (,) k <$> fullNormalize v

normalize (TProj _ t@(TRecord _ fs) k) | isVal t = return $ Map.lookup k fs
normalize (TProj p t@(TRecord _ _) k) = fmap(flip(TProj p) k) <$> normalize t

normalize (TTag p l t ty) = fmap(flip(TTag p l) ty) <$> normalize t

normalize (TCase _ (TTag _ key v _) branches) | isVal v =
    return $ fmap (\(_, t) -> termSubstitutionTop v t) (Map.lookup key branches)

normalize (TCase p t fields) = fmap(flip(TCase p) fields) <$> normalize t

normalize (TAscribe _ t _) | isVal t = ok t
normalize (TAscribe x t ty) = fmap(flip(TAscribe x) ty) <$> normalize t

normalize (TRef p t) | isVal t = (\x -> return $ TLoc p x) <$> extend t
normalize (TRef p t) = fmap(TRef p) <$> normalize t

normalize (TDeref _ (TLoc _ l)) = return <$> deref l
normalize (TDeref p t) = fmap(TDeref p) <$> normalize t

normalize (TAssign p (TLoc _ l) t2) | isVal t2 = assign l t2 >> ok (TUnit p)
normalize (TAssign p t1 t2) | isVal t1 = fmap(TAssign p t1) <$> normalize t2
normalize (TAssign p t1 t2)  = fmap(flip(TAssign p) t2) <$> normalize t1

normalize (TError p) = lift $ throwE $ show p <> ": error"

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) = ok $ TFloat p (t1 * t2)
normalize (TTimesFloat p t1 t2) | isVal t1 = fmap(TTimesFloat p t1) <$> normalize t2
normalize (TTimesFloat p t1 t2) = fmap(flip (TTimesFloat p) t2) <$> normalize t1

normalize (TSucc p t) = fmap(TSucc p) <$> normalize t

normalize (TPred _ (TZero p)) = ok $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = ok t
normalize (TPred p t) = fmap(TPred p) <$> normalize t

normalize (TIsZero _ (TZero p)) = ok $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = ok $ TFalse p
normalize (TIsZero p t) = fmap(TIsZero p) <$> normalize t

normalize (TTApp _ (TTAbs _ x _ t11) tyT2) = ok $ typeTermSubstitutionTop tyT2 t11
normalize (TTApp p t1 tyT2) = fmap(flip (TTApp p) tyT2) <$> normalize t1

normalize (TApp _ (TAbs _ _ tyT11 t12) v2) | isVal v2 = ok $ termSubstitutionTop v2 t12
normalize (TApp p v1 t2) | isVal v1 = fmap(TApp p v1) <$> normalize t2
normalize (TApp p t1 t2) = fmap(flip (TApp p) t2) <$> normalize t1

normalize _ = nvm
