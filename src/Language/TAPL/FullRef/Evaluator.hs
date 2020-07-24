module Language.TAPL.FullRef.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Lazy as Map

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Language.TAPL.FullRef.Types
import Language.TAPL.FullRef.Parser
import Language.TAPL.FullRef.TypeChecker
import Language.TAPL.FullRef.Pretty

type Eval a = StateT LCMemory Maybe a

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right ([], _) -> return ""
    Right (ast, names) -> do
        let mem = [] :: LCMemory
        _ <- sequence $ typeOf names mem <$> ast
        let (ast', mem') = eval mem ast
            result = last $ ast'
        ty <- typeOf names mem' result
        result' <- render names result
        return $ result' ++ ":" ++ (show $ pretty ty)

eval :: LCMemory -> AST -> (AST, LCMemory)
eval mem ast = f [] mem ast
    where f acc m [] = (reverse acc, m)
          f acc m (t:ts) = f (t':acc) m' ts where (t', m') = fullNormalize m t

fullNormalize :: LCMemory -> Term -> (Term, LCMemory)
fullNormalize m t =
    case runStateT (normalize t) m of
         Just (t', m') -> fullNormalize m' t'
         Nothing -> (t, m)

normalize :: Term -> Eval Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf info t1 t2 t3) = normalize t1 >>= \t1' -> return $ TIf info t1' t2 t3

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp info t1 t2) | isVal t1 = TApp info t1 <$> normalize t2
normalize (TApp info t1 t2) = normalize t1 >>= \t1' -> return $ TApp info t1' t2

normalize (TSucc info t) = TSucc info <$> normalize t

normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred info t) = TPred info <$> normalize t

normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t = return $ TFalse info
normalize (TIsZero info t) = TIsZero info <$> normalize t

normalize (TRef info t) | isVal t = do
    mem <- get
    let (location, mem') = Language.TAPL.FullRef.Types.extend mem t
    put mem'
    return $ TLoc info location

normalize (TRef info t) = TRef info <$> normalize t

normalize (TDeref _ (TLoc _ location)) = do
    mem <- get
    return $ Language.TAPL.FullRef.Types.lookup mem location

normalize (TDeref info t) = TDeref info <$> normalize t

normalize (TAssign info t1 t2) | not (isVal t1) = normalize t1 >>= \t1' -> return $ TAssign info t1' t2
normalize (TAssign info t1 t2) | not $ isVal t2 = TAssign info t1 <$> normalize t2
normalize (TAssign info (TLoc _ location) t2) = do
    mem <- get
    put $ update mem location t2
    return $ TUnit info

normalize (TLet _ _ t1 t2) | isVal t1 = return $ substitutionTop t1 t2
normalize (TLet info v t1 t2) = normalize t1 >>= \t1' -> return $ TLet info v t1' t2

normalize (TAscribe _ t _) | isVal t = return t
normalize (TAscribe _ t _) = normalize t

normalize (TPair info t1 t2) | isVal t1 = TPair info t1 <$> normalize t2
normalize (TPair info t1 t2) = normalize t1 >>= \t1' -> return $ TPair info t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = lift Nothing
normalize t@(TRecord _ _) | isVal t = lift Nothing
normalize (TRecord info fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields
    return $ TRecord info (Map.fromList fields')
    where evalField (k, field) | isVal field = return (k, field)
          evalField (k, field) = do
            field' <- normalize field
            return (k, field')

normalize (TProj _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = lift $ Map.lookup key fields
normalize (TProj info t@(TRecord _ _) (TKeyword x key)) = do
    t' <- normalize t
    return $ (TProj info t' (TKeyword x key))

normalize (TProj _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TProj _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t
normalize (TProj info t k) = normalize t >>= \t' -> return $ TProj info t' k

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = return $ substitutionTop t1 t2
normalize (TFix info t) = TFix info <$> normalize t

normalize (TCase _ (TTag _ key v _) branches) | isVal v = do
    case Map.lookup key branches of
         Just (_, t) -> return $ substitutionTop v t
         Nothing -> lift Nothing

normalize (TCase info t fields) = normalize t >>= \t' -> return $ TCase info t' fields
normalize _ = lift Nothing
