module Language.TAPL.SimpleBool.Evaluator (evalString) where

import Language.TAPL.SimpleBool.Types
import Language.TAPL.SimpleBool.Parser
import Language.TAPL.SimpleBool.TypeChecker
import Language.TAPL.SimpleBool.Pretty

import Data.List (last)

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
      _ <- sequence $ typeOf names <$> ast
      let result = last $ eval ast
      ty <- typeOf names result
      result' <- render names result
      return $ result' ++ ":" ++ (show $ pretty ty)

eval :: AST -> AST
eval ast = fullNormalize <$> ast

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

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t

normalize (TApp info t1 t2) | isVal t1  = do
    t2' <- normalize t2
    return $ TApp info t1 t2'

normalize (TApp info t1 t2) = do
    t1' <- normalize t1
    return $ TApp info t1' t2

normalize _ =  Nothing

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t1) = TAbs info x ty (walk (c+1) t1)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk _ (TTrue info) = TTrue info
                  walk _ (TFalse info) = TFalse info

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
