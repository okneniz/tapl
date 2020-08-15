module Language.TAPL.FullRecon.Evaluator (evalString) where

import Data.List (last)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileJust)
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
        Right (commands, names) -> runExcept (evalStateT (f commands) (newState names))
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
   modify $ \s -> s { names = (bind name binding (names s)) }
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
normalize (TIf info t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf info t1' t2 t3
normalize (TSucc info t1) = TSucc info <$> normalize t1
normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t
normalize (TPred info t) = TPred info <$> normalize t
normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t = return $ TFalse info
normalize (TIsZero info t) = TIsZero info <$> normalize t
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp info t1 t2) | isVal t1 = TApp info t1 <$> normalize t2
normalize (TApp info t1 t2) = normalize t1 >>= \t1' -> return $ TApp info t1' t2
normalize (TLet info v t1 t2) | isVal t1 = return $ termSubstitutionTop t1 t2
normalize (TLet info v t1 t2) = normalize t1 >>= \t1' -> return $ TLet info v t1' t2
normalize _ = Nothing
