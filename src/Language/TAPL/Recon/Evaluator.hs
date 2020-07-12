module Language.TAPL.Recon.Evaluator (evalString) where

import Data.List (last)

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Text.Parsec (SourcePos)

import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Parser
import Language.TAPL.Recon.Context
import Language.TAPL.Recon.TypeChecker
import Language.TAPL.Recon.Pretty

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
    let ts' = whileJust normalize <$> ts
    cs' <- evalCommands cs
    return $ ts' ++ cs'

typeCheck :: AST -> Eval Type
typeCheck [t] = typeOf t
typeCheck (t:ts) = typeOf t >> typeCheck ts

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf pos t1 t2 t3) = do
    t1' <- normalize t1
    return $ TIf pos t1' t2 t3

normalize (TSucc pos t1) = TSucc pos <$> normalize t1

normalize (TPred _ (TZero pos)) = return $ TZero pos
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t
normalize (TPred pos t) = TPred pos <$> normalize t

normalize (TIsZero _ (TZero pos)) = return $ TTrue pos
normalize (TIsZero _ (TSucc pos t)) | isNumerical t = return $ TFalse pos
normalize (TIsZero pos t) = TIsZero pos <$> normalize t

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp pos t1 t2) | isVal t1 = TApp pos t1 <$> normalize t2
normalize (TApp pos t1 t2) = do
    t1' <- normalize t1
    return $ TApp pos t1' t2

normalize _ = Nothing

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal x | isNumerical x = True
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ x) = isNumerical x
isNumerical _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar pos name depth) = onVar c pos name depth
                           walk c (TAbs pos x ty t1) = TAbs pos x (onType c ty) (walk (c+1) t1)
                           walk c (TApp pos t1 t2) = TApp pos (walk c t1) (walk c t2)
                           walk c (TIf pos t1 t2 t3) = TIf pos (walk c t1) (walk c t2) (walk c t3)
                           walk _ (TTrue pos) = TTrue pos
                           walk _ (TFalse pos) = TFalse pos
                           walk _ (TZero pos) = TZero pos
                           walk c (TIsZero pos t1) = TIsZero pos (walk c t1)
                           walk c (TPred pos t1) = TPred pos (walk c t1)
                           walk c (TSucc pos t1) = TSucc pos (walk c t1)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = termMap onVar (typeShiftAbove d) s t
                 where onVar c pos name depth | name >= c = TVar pos (name + d) (depth + d)
                       onVar _ pos name depth = TVar pos name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType 0 t
                   where onVar c _ name _ | name == j + c = termShift c s
                         onVar _ pos name depth = TVar pos name depth
                         onType _ ty = ty

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
