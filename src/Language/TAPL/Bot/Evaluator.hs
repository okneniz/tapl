module Language.TAPL.Bot.Evaluator (evalString) where

import Data.List (last)

import Language.TAPL.Common.Helpers (whileJust)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Parser
import Language.TAPL.Bot.Context
import Language.TAPL.Bot.TypeChecker
import Language.TAPL.Bot.Pretty

evalString :: String -> String -> Either String String
evalString code source = do
    case parse source code of
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
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp pos t1 t2) | isVal t1 = TApp pos t1 <$> normalize t2
normalize (TApp pos t1 t2) = normalize t1 >>= \t1' -> return $ TApp pos t1' t2
normalize _ = Nothing
