module Language.TAPL.FullFSub.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileM, ok, nvm)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.FullFSub.Types
import Language.TAPL.FullFSub.Parser
import Language.TAPL.FullFSub.Context
import Language.TAPL.FullFSub.TypeChecker
import Language.TAPL.FullFSub.Pretty

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, names) -> runExcept $ evalStateT (f commands) names
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
normalize (TApp _ (TAbs p _ tyT11 t12) v2) | isVal v2 = Just <$> termSubstitutionTop p v2 t12
normalize (TApp p v1 t2) | isVal v1 = fmap(TApp p v1) <$> normalize t2
normalize (TApp p t1 t2) = fmap(flip (TApp p) t2) <$> normalize t1

normalize (TIf _ (TTrue _) t _ ) = ok t
normalize (TIf _ (TFalse _) _ t) = ok t
normalize (TIf p t1 t2 t3) = fmap(\t1' -> TIf p t1' t2 t3) <$> normalize t1

normalize (TRecord _ fields) | (Map.size fields) == 0 = nvm
normalize t@(TRecord _ _) | isVal t = nvm
normalize (TRecord p fs) = do
    fs' <- mapM evalField $ Map.toList fs
    ok $ TRecord p (Map.fromList fs')
    where evalField (k,v) = (,) k <$> fullNormalize v

normalize (TProj _ t@(TRecord _ fs) k) | isVal t = return $ Map.lookup k fs
normalize (TProj p t@(TRecord _ _) k) = fmap(flip(TProj p) k) <$> normalize t

normalize (TLet p _ t1 t2) | isVal t1 = Just <$> termSubstitutionTop p t1 t2
normalize (TLet p v t1 t2) = fmap(flip(TLet p v) t2) <$> normalize t1

normalize t1@(TFix _ a@(TAbs p _ _ t2)) | isVal a = Just <$> termSubstitutionTop p t1 t2
normalize (TFix p t) = fmap(TFix p) <$> normalize t

normalize (TAscribe _ t _) | isVal t = ok t
normalize (TAscribe _ t _) = normalize t

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) = ok $ TFloat p (t1 * t2)
normalize (TTimesFloat p t1 t2) | isVal t1 = fmap(TTimesFloat p t1) <$> normalize t2
normalize (TTimesFloat p t1 t2) = fmap(flip (TTimesFloat p) t2) <$> normalize t1

normalize (TTApp _ (TTAbs p x _ t11) tyT2) = Just <$> typeTermSubstitutionTop p tyT2 t11
normalize (TTApp p t1 tyT2) = fmap(flip (TTApp p) tyT2) <$> normalize t1

normalize (TSucc p t) = fmap(TSucc p) <$> normalize t

normalize (TPred _ (TZero p)) = ok $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = ok t
normalize (TPred p t) = fmap(TPred p) <$> normalize t

normalize (TIsZero _ (TZero p)) = ok $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = ok $ TFalse p
normalize (TIsZero p t) = fmap(TIsZero p) <$> normalize t

normalize (TUnpack _ _ _ (TPack p2 tyT11 v12 _) t2) | isVal v12 = do
    x <- termShift p2 1 v12
    y <- termSubstitutionTop p2 x t2
    Just <$> typeTermSubstitutionTop p2 tyT11 y

normalize (TUnpack p ty x t1 t2) = fmap(flip(TUnpack p ty x) t2) <$> normalize t1

normalize (TPack p ty1 t2 ty3) = fmap(flip(TPack p ty1) ty3) <$> normalize t2
normalize _ = nvm
