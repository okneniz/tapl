module Language.TAPL.Untyped.Evaluator (evalString) where

import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Parser
import Language.TAPL.Untyped.Pretty

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
        let result = last $ eval ast
        result' <- render names result
        return result'

eval :: AST -> AST
eval ast = fullNormalize <$> ast

fullNormalize :: Term -> Term
fullNormalize t = case normalize t of
                       Just t' -> fullNormalize t'
                       Nothing -> t

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ t) v) | isVal v = return $ substitutionTop v t

normalize (TApp info t1 t2) | isVal t1  = do
    t2' <- normalize t2
    return $ TApp info t1 t2'

normalize (TApp info t1 t2) = do
    t1' <- normalize t1
    return $ TApp info t1' t2

normalize _ = Nothing

isVal :: Term -> Bool
isVal (TAbs _ _ _) = True
isVal _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x t1) = TAbs info x (walk (c+1) t1)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = tmmap onvar s t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar _ info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c _ name _ | name == j + c = shift c s
                     onvar _ info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
