module Language.TAPL.FullEquirec.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Strict as Map

import Control.Monad (liftM)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.FullEquirec.Types
import Language.TAPL.FullEquirec.Parser
import Language.TAPL.FullEquirec.Context
import Language.TAPL.FullEquirec.TypeChecker
import Language.TAPL.FullEquirec.Pretty

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
normalize (TIf _ (TTrue _) t _ ) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf info t1 t2 t3) = do
    t1' <- normalize t1
    return $ TIf info t1' t2 t3

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v =
    return $ termSubstitutionTop v t

normalize (TApp info t1 t2) | isVal t1 =
    TApp info t1 <$> normalize t2

normalize (TApp info t1 t2) = do
    t1' <- normalize t1
    return $ TApp info t1' t2

normalize (TSucc info t) = TSucc info <$> normalize t

normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred info t) = TPred info <$> normalize t

normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t =
    return $ TFalse info

normalize (TIsZero info t) = TIsZero info <$> normalize t

normalize (TPair info t1 t2) | isVal t1 =
    TPair info t1 <$> normalize t2

normalize (TPair info t1 t2) = do
    t1' <- normalize t1
    return $ TPair info t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord info fs) = do
    fs' <- sequence $ evalField <$> Map.toList fs
    return $ TRecord info (Map.fromList fs')
    where evalField (k, v) | isVal v = return (k, v)
          evalField (k, v) = do
            v' <- normalize v
            return (k, v')

normalize (TLookup _ t@(TRecord _ fs) (TKeyword _ k)) | isVal t =
    Map.lookup k fs

normalize (TLookup info t@(TRecord _ _) (TKeyword x k)) = do
    t' <- normalize t
    return $ (TLookup info t' (TKeyword x k))

normalize (TLookup _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TLookup _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t

normalize (TLookup info t k) = do
    t' <- normalize t
    return $ TLookup info t' k

normalize (TLet _ _ t1 t2) | isVal t1 =
    return $ termSubstitutionTop t1 t2

normalize (TLet info v t1 t2) = do
    t1' <- normalize t1
    return $ TLet info v t1' t2

normalize (TAscribe _ t _) | isVal t = return t
normalize (TAscribe _ t _) = normalize t

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a =
    return $ termSubstitutionTop t1 t2

normalize (TFix info t) = TFix info <$> normalize t

normalize (TTimesFloat info (TFloat _ t1) (TFloat _ t2)) =
    return $ TFloat info (t1 * t2)

normalize (TTimesFloat info t1@(TFloat _ _) t2) =
    TTimesFloat info t1 <$> normalize t2

normalize (TTimesFloat info t1 t2@(TFloat _ _)) = do
    t1' <- normalize t1
    return $ TTimesFloat info t1' t2

normalize (TCase _ (TTag _ k v _) bs) | isVal v =
    liftM (\(_, t) -> termSubstitutionTop v t)
          (Map.lookup k bs)

normalize (TCase info t fs) = do
    t' <- normalize t
    return $ TCase info t' fs

normalize _ = Nothing
