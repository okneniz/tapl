module Language.TAPL.RcdSubBot.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.RcdSubBot.Types
import Language.TAPL.RcdSubBot.Parser
import Language.TAPL.RcdSubBot.Context
import Language.TAPL.RcdSubBot.TypeChecker
import Language.TAPL.RcdSubBot.Pretty

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
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp pos t1 t2) | isVal t1 = TApp pos t1 <$> normalize t2
normalize (TApp pos t1 t2) = normalize t1 >>= \t1' -> return $ TApp pos t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord pos fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields -- не то!
    return $ TRecord pos (Map.fromList fields')
    where evalField (k, field) | isVal field = return (k, field)
          evalField (k, field) = do
            field' <- normalize field
            return (k, field')

normalize (TProj _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = Map.lookup key fields
normalize (TProj pos t@(TRecord _ _) (TKeyword x key)) = do
    t' <- normalize t
    return $ (TProj pos t' (TKeyword x key))

normalize _ = Nothing
