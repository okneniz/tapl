module Language.TAPL.TypedArith.Evaluator (evalString) where

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Common.Context
import Language.TAPL.TypedArith.Context
import Language.TAPL.TypedArith.Types
import Language.TAPL.TypedArith.Parser
import Language.TAPL.TypedArith.TypeChecker
import Language.TAPL.TypedArith.Pretty

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
                    return $ show t' <> ":" <> show ty'

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
    return $ ts' <> cs'

typeCheck :: AST -> Eval Type
typeCheck [] = lift $ throwE "attempt to check empty AST"
typeCheck [t] = typeOf t
typeCheck (t:ts) = typeOf t >> typeCheck ts

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _ ) = return t
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
normalize _ = Nothing

