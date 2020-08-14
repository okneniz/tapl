module Language.TAPL.FullSimple.Evaluator (evalString) where

import qualified Data.Map.Lazy as Map
import Control.Monad (liftM)

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Parser
import Language.TAPL.FullSimple.TypeChecker
import Language.TAPL.FullSimple.Pretty
import Language.TAPL.FullSimple.Context

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except

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
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf p t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf p t1' t2 t3

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = normalize t1 >>= \t1' -> return $ TApp p t1' t2

normalize (TSucc p t) = TSucc p <$> normalize t

normalize (TPred _ (TZero p)) = return $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred p t) = TPred p <$> normalize t

normalize (TIsZero _ (TZero p)) = return $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = return $ TFalse p
normalize (TIsZero p t) = TIsZero p <$> normalize t

normalize (TPair p t1 t2) | isVal t1 = TPair p t1 <$> normalize t2
normalize (TPair p t1 t2) = normalize t1 >>= \t1' -> return $ TPair p t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord p fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields
    return $ TRecord p (Map.fromList fields')
    where evalField (k, field) | isVal field = return (k, field)
          evalField (k, field) = do
            field' <- normalize field
            return (k, field')

normalize (TProj _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = Map.lookup key fields
normalize (TProj p t@(TRecord _ _) (TKeyword x key)) = do
    t' <- normalize t
    return $ TProj p t' (TKeyword x key)

normalize (TProj _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TProj _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t

normalize (TProj p t k) = normalize t >>= \t' -> return $ TProj p t' k

normalize (TLet _ _ t1 t2) | isVal t1 = return $ substitutionTop t1 t2
normalize (TLet p v t1 t2) = normalize t1 >>= \t1' -> return $ TLet p v t1' t2

normalize (TAscribe _ t _) = return t

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = return $ substitutionTop t1 t2
normalize (TFix p t) = TFix p <$> normalize t

normalize (TCase _ (TTag _ key v _) branches) | isVal v =
    liftM (\(_, t) -> substitutionTop v t)
          (Map.lookup key branches)

normalize (TCase p t fields) = normalize t >>= \t' -> return $ TCase p t' fields

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) =
    return $ TFloat p (t1 * t2)

normalize (TTimesFloat p t1@(TFloat _ _) t2) =
    TTimesFloat p t1 <$> normalize t2

normalize (TTimesFloat p t1 t2@(TFloat _ _)) = do
    t1' <- normalize t1
    return $ TTimesFloat p t1' t2

normalize _ = Nothing
