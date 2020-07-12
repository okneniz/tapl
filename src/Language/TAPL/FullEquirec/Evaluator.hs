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
normalize (TIf p t1 t2 t3) = do
    t1' <- normalize t1
    return $ TIf p t1' t2 t3

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v =
    return $ termSubstitutionTop v t

normalize (TApp p t1 t2) | isVal t1 =
    TApp p t1 <$> normalize t2

normalize (TApp p t1 t2) = do
    t1' <- normalize t1
    return $ TApp p t1' t2

normalize (TSucc p t) = TSucc p <$> normalize t

normalize (TPred _ (TZero p)) = return $ TZero p
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred p t) = TPred p <$> normalize t

normalize (TIsZero _ (TZero p)) = return $ TTrue p
normalize (TIsZero _ (TSucc p t)) | isNumerical t =
    return $ TFalse p

normalize (TIsZero p t) = TIsZero p <$> normalize t

normalize (TPair p t1 t2) | isVal t1 =
    TPair p t1 <$> normalize t2

normalize (TPair p t1 t2) = do
    t1' <- normalize t1
    return $ TPair p t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing
normalize t@(TRecord _ _) | isVal t = Nothing

normalize (TRecord p fs) = do
    fs' <- sequence $ evalField <$> Map.toList fs
    return $ TRecord p (Map.fromList fs')
    where evalField (k, v) | isVal v = return (k, v)
          evalField (k, v) = do
            v' <- normalize v
            return (k, v')

normalize (TLookup _ t@(TRecord _ fs) (TKeyword _ k)) | isVal t =
    Map.lookup k fs

normalize (TLookup p t@(TRecord _ _) (TKeyword x k)) = do
    t' <- normalize t
    return $ (TLookup p t' (TKeyword x k))

normalize (TLookup _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TLookup _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t

normalize (TLookup p t k) = do
    t' <- normalize t
    return $ TLookup p t' k

normalize (TLet _ _ t1 t2) | isVal t1 =
    return $ termSubstitutionTop t1 t2

normalize (TLet p v t1 t2) = do
    t1' <- normalize t1
    return $ TLet p v t1' t2

normalize (TAscribe _ t _) | isVal t = return t
normalize (TAscribe _ t _) = normalize t

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a =
    return $ termSubstitutionTop t1 t2

normalize (TFix p t) = TFix p <$> normalize t

normalize (TTimesFloat p (TFloat _ t1) (TFloat _ t2)) =
    return $ TFloat p (t1 * t2)

normalize (TTimesFloat p t1@(TFloat _ _) t2) =
    TTimesFloat p t1 <$> normalize t2

normalize (TTimesFloat p t1 t2@(TFloat _ _)) = do
    t1' <- normalize t1
    return $ TTimesFloat p t1' t2

normalize (TCase _ (TTag _ k v _) bs) | isVal v =
    liftM (\(_, t) -> termSubstitutionTop v t)
          (Map.lookup k bs)

normalize (TCase p t fs) = do
    t' <- normalize t
    return $ TCase p t' fs

normalize _ = Nothing
