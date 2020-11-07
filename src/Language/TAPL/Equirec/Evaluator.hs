module Language.TAPL.Equirec.Evaluator (evalString) where

import Data.List (last)

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Common.Context (bind)
import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Parser
import Language.TAPL.Equirec.Context
import Language.TAPL.Equirec.TypeChecker
import Language.TAPL.Equirec.Pretty

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
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp p t1 t2) | isVal t1 = TApp p t1 <$> normalize t2
normalize (TApp p t1 t2) = normalize t1 >>= \t1' -> return $ TApp p t1' t2
normalize _ = Nothing
