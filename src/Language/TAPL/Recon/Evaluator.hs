module Language.TAPL.Recon.Evaluator (evalString) where

import Data.List (last)

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

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

normalize (TSucc info t1) = do
    t' <- normalize t1
    return $ TSucc info t'

normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t

normalize (TPred info t) = do
    t' <- normalize t
    return $ TPred info t'

normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t = return $ TFalse info

normalize (TIsZero info t) = do
    t' <- normalize t
    return $ TIsZero info t'

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t

normalize (TApp info t1 t2) | isVal t1 = do
    t2' <- normalize t2
    return $ TApp info t1 t2'

normalize (TApp info t1 t2) = do
    t1' <- normalize t1
    return $ TApp info t1' t2

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

termMap :: (Int -> Info -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar info name depth) = onVar c info name depth
                           walk c (TAbs info x ty t1) = TAbs info x (onType c ty) (walk (c+1) t1)
                           walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                           walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                           walk _ (TTrue info) = TTrue info
                           walk _ (TFalse info) = TFalse info
                           walk _ (TZero info) = TZero info
                           walk c (TIsZero info t1) = TIsZero info (walk c t1)
                           walk c (TPred info t1) = TPred info (walk c t1)
                           walk c (TSucc info t1) = TSucc info (walk c t1)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = termMap onVar (typeShiftAbove d) s t
                 where onVar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onVar _ info name depth = TVar info name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType 0 t
                   where onVar c _ name _ | name == j + c = termShift c s
                         onVar _ info name depth = TVar info name depth
                         onType _ ty = ty

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
