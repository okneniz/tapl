module Language.TAPL.RcdSubBot.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Strict as Map

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.RcdSubBot.Types
import Language.TAPL.RcdSubBot.Parser
import Language.TAPL.RcdSubBot.Context
import Language.TAPL.RcdSubBot.TypeChecker
import Language.TAPL.RcdSubBot.Pretty

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
    let ts' = fullNormalize <$> ts
    cs' <- evalCommands cs
    return $ ts' ++ cs'

typeCheck :: AST -> Eval Type
typeCheck [] = lift $ throwE "attempt to check empty AST"
typeCheck [t] = typeOf t
typeCheck (t:ts) = typeOf t >> typeCheck ts

fullNormalize :: Term -> Term
fullNormalize t = case normalize t of
                       Just t' -> fullNormalize t'
                       Nothing -> t

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp info t1 t2) | isVal t1 = TApp info t1 <$> normalize t2
normalize (TApp info t1 t2) = normalize t1 >>= \t1' -> return $ TApp info t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord info fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields -- не то!
    return $ TRecord info (Map.fromList fields')
    where evalField (k, field) | isVal field = return (k, field)
          evalField (k, field) = do
            field' <- normalize field
            return (k, field')

normalize (TProj _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = Map.lookup key fields
normalize (TProj info t@(TRecord _ _) (TKeyword x key)) = do
    t' <- normalize t
    return $ (TProj info t' (TKeyword x key))

normalize _ = Nothing
