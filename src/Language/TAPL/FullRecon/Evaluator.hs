module Language.TAPL.FullRecon.Evaluator (evalString) where

import Data.List (last)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileJust)
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
    where
        f cs = do
            cs' <- evalCommands cs
            if null cs'
            then return ""
            else do let t = last cs'
                    ty <- typeOf t
                    t' <- prettify t
                    ty' <- prettifyType ty
                    return $ show t' <> ":" <> show ty'

evalCommands :: [Command] -> Eval AST
evalCommands [] = return []
evalCommands ((Bind _ name binding):cs) = do
   modify $ \s -> s { names = (bind name binding (names s)) }
   evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval ts):cs) = do
    _ <- typeCheck ts
    let ts' = whileJust normalize <$> ts
    cs' <- evalCommands cs
    return $ ts' <> cs'

typeCheck :: AST -> Eval Type
typeCheck [] = lift $ throwE "attempt to check empty AST"
typeCheck [t] = typeOf t
typeCheck (t:ts) = typeOf t >> typeCheck ts

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf p t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf p t1' t2 t3
normalize (TSucc p t1) = TSucc p <$> normalize t1
normalize (TPred _ (TZero p)) = return $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t
normalize (TPred p t) = TPred p <$> normalize t
normalize (TIsZero _ (TZero p)) = return $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t = return $ TFalse p
normalize (TIsZero p t) = TIsZero p <$> normalize t
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = normalize t1 >>= \t1' -> return $ TApp p t1' t2
normalize (TLet _ _ t1 t2) | isVal t1 = return $ termSubstitutionTop t1 t2
normalize (TLet p v t1 t2) = normalize t1 >>= \t1' -> return $ TLet p v t1' t2
normalize _ = Nothing
