module Language.TAPL.FullRecon.Evaluator (evalString) where

import Data.List (last)

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Language.TAPL.FullRecon.Types
import Language.TAPL.FullRecon.Parser
import Language.TAPL.FullRecon.Context
import Language.TAPL.FullRecon.TypeChecker
import Language.TAPL.FullRecon.Pretty

evalString :: String -> String -> Either String String
evalString code source = do
    case parse source code of
        Left e -> Left $ show e
        Right (commands, names) -> evalState (runExceptT (runReaderT (f commands) names)) (0, [])
    where
        f cs = do
            cs' <- evalCommands cs
            let t = last cs'
            ty <- typeOf t
            t' <- render t
            return $ t' ++ ":" ++ (show $ pretty ty)

evalCommands :: [Command] -> Eval AST
evalCommands [] = return []
evalCommands ((Bind _ name binding):cs) = local (bind name binding) (evalCommands cs)
evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval ts):cs) = do
    _ <- typeCheck ts
    let ts' = fullNormalize <$> ts
    cs' <- evalCommands cs
    return $ ts' ++ cs'

typeCheck :: AST -> Eval Type
typeCheck [t] = typeOf t
typeCheck (t:ts) = typeOf t >> typeCheck ts

fullNormalize :: Term -> Term
fullNormalize t = case normalize t of
                       Just t' -> fullNormalize t'
                       Nothing -> t

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf info t1 t2 t3) = do
    t1' <- normalize t1
    return $ TIf info t1' t2 t3

normalize (TSucc info t1) = TSucc info <$> normalize t1

normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t
normalize (TPred info t) = TPred info <$> normalize t

normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t = return $ TFalse info
normalize (TIsZero info t) = TIsZero info <$> normalize t

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v =
    return $ termSubstitutionTop v t

normalize (TApp info t1 t2) | isVal t1 = TApp info t1 <$> normalize t2
normalize (TApp info t1 t2) = do
    t1' <- normalize t1
    return $ TApp info t1' t2

normalize (TLet info v t1 t2) | isVal t1 =
    return $ termSubstitutionTop t1 t2

normalize (TLet info v t1 t2) = do
    t1' <- normalize t1
    return $ TLet info v t1' t2

normalize _ = Nothing
