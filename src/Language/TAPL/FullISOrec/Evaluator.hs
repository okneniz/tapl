module Language.TAPL.FullISOrec.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad (liftM)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.FullISOrec.Types
import Language.TAPL.FullISOrec.Parser
import Language.TAPL.FullISOrec.Context
import Language.TAPL.FullISOrec.TypeChecker
import Language.TAPL.FullISOrec.Pretty

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
        Left e -> Left $ show e
        Right ([], _) -> return ""
        Right (commands, names) -> runExcept (evalStateT (f commands) names)
    where
        f cs = do
            cs' <- evalCommands cs
            if null cs'
            then return ""
            else do let t = last cs'
                    ty <- typeOf t
                    t' <- prettify t
                    ty' <- prettifyType ty
                    return $ show t' ++ ":" ++ show ty'

evalCommands :: [Command] -> Eval AST
evalCommands [] = return []
evalCommands ((Bind _ name binding):cs) = do
   modify $ bind name binding
   evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval ts):cs) = do
    _ <- typeCheck ts
    let ts' = whileJust normalize <$> ts
    cs' <- evalCommands cs
    return $ ts' ++ cs'

typeCheck :: AST -> Eval Type
typeCheck [] = lift $ throwE "attempt to check empty AST"
typeCheck [t] = typeOf t
typeCheck (t:ts) = typeOf t >> typeCheck ts

normalize :: Term -> Maybe Term
normalize (TLet _ _ t1 t2) | isVal t1 = return $ termSubstitutionTop t1 t2
normalize (TLet p v t1 t2) = normalize t1 >>= \t1' -> return $ TLet p v t1' t2

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = return $ termSubstitutionTop t1 t2
normalize (TFix p t) = TFix p <$> normalize t

normalize (TIf _ (TTrue _) t _ ) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf p t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf p t1' t2 t3

normalize (TAscribe _ t _) | isVal t = return t
normalize (TAscribe _ t _) = normalize t

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing
normalize (TRecord p fs) = do
    fs' <- sequence $ evalField <$> Map.toList fs
    return $ TRecord p (Map.fromList fs')
    where evalField (k, v) | isVal v = return (k, v)
          evalField (k, v) = ((,) k) <$> normalize v

normalize (TProj _ t@(TRecord _ fs) (TKeyword _ k)) | isVal t = Map.lookup k fs
normalize (TProj p t@(TRecord _ _) (TKeyword x k)) = normalize t >>= \t' -> return $ (TProj p t' (TKeyword x k))

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) = return $ TFloat p (t1 * t2)
normalize (TTimesFloat p t1 t2) | isVal t1 = TTimesFloat p t1 <$> normalize t2
normalize (TTimesFloat p t1 t2) = normalize t1 >>= \t1' -> return $ TTimesFloat p t1' t2

normalize (TApp _ (TUnfold _ tyS) (TApp _ (TFold _ tyT) v)) | isVal v = return v
normalize (TApp p t1@(TFold _ _) t2) = TApp p t1 <$> normalize t2
normalize (TApp p t1@(TUnfold _ _) t2) = TApp p t1 <$> normalize t2
normalize (TApp _ (TAbs _ x tyT11 t12) v2) | isVal v2 = return $ termSubstitutionTop v2 t12
normalize (TApp p v1 t2) | isVal v1 = TApp p v1 <$> normalize t2
normalize (TApp p t1 t2) = flip (TApp p) t2 <$> normalize t1

normalize (TSucc p t) = TSucc p <$> normalize t

normalize (TPred _ (TZero p)) = return $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred p t) = TPred p <$> normalize t

normalize (TIsZero _ (TZero p)) = return $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = return $ TFalse p

normalize (TIsZero p t) = TIsZero p <$> normalize t

normalize (TProj _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TProj _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t
normalize (TProj p t k) = normalize t >>= \t' -> return $ TProj p t' k

normalize (TTag p x t ty) = normalize t >>= \t' -> return $ TTag p x t' ty

normalize (TCase _ (TTag _ k v _) bs) | isVal v = liftM (\(_, t) -> termSubstitutionTop v t) (Map.lookup k bs)
normalize (TCase p t fs) = normalize t >>= \t' -> return $ TCase p t' fs

normalize (TPair p t1 t2) | isVal t1 = TPair p t1 <$> normalize t2
normalize (TPair p t1 t2) = normalize t1 >>= \t1' -> return $ TPair p t1' t2

normalize _ = Nothing
