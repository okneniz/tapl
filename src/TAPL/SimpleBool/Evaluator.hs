{-# LANGUAGE FlexibleContexts #-}

module TAPL.SimpleBool.Evaluator where

import TAPL.SimpleBool.Types
import TAPL.SimpleBool.Parser
import TAPL.SimpleBool.Context
import TAPL.SimpleBool.TypeChecker
import Data.List (findIndex, intercalate, all, nub, (\\), last)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

eval :: String -> String -> Either EvaluationError String
eval code path = do
  case (parse path code) of
    Left e -> Left $ ParsecError e
    Right c@(SimpleBoolContext ns ast) ->
      if correctAST c
      then case last $ (\t -> fullNormalize $ SimpleBoolContext ns t) <$> ast of
                result -> case typeOf result of
                               Right ty -> return $ show result ++ ":" ++ show ty
                               Left x -> Left $ TypeError $ show x
      else Left $ TypeError $ show $ head $ typeErrors c

fromLeft (Left x) = x
fromLeft (Right x) = undefined

correctAST :: SimpleBoolContext AST -> Bool
correctAST (SimpleBoolContext _ []) = False
correctAST c = all isRight $ allTypes c

typeErrors :: SimpleBoolContext AST -> [TypeError]
typeErrors (SimpleBoolContext _ []) = []
typeErrors c = map fromLeft $ filter isLeft $ allTypes c

allTypes :: SimpleBoolContext AST -> [Either TypeError Type]
allTypes (SimpleBoolContext ns ast) = fmap f ast where f t = typeOf (SimpleBoolContext ns t)

fullNormalize :: SimpleBoolContext Term -> SimpleBoolContext Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: SimpleBoolContext Term -> Maybe (SimpleBoolContext Term)
normalize c@(SimpleBoolContext _ (TIf _ (TTrue _) t _ )) = return $ c `withTerm` t
normalize c@(SimpleBoolContext _ (TIf _ (TFalse _) _ t)) = return $ c `withTerm` t

normalize c@(SimpleBoolContext _ (TIf info t1 t2 t3)) = do
  (SimpleBoolContext _ t1') <- normalize $ c `withTerm` t1
  return $ c `withTerm` (TIf info t1' t2 t3)

normalize c@(SimpleBoolContext ns (TApp _ (TAbs _ _ _ t) v)) | isVal v =
    return $ SimpleBoolContext ns (substitutionTop v t)

normalize c@(SimpleBoolContext ns (TApp info t1 t2)) | isVal t1  = do
    (SimpleBoolContext ns t2') <- normalize $ c `withTerm` t2
    return $ SimpleBoolContext ns (TApp info t1 t2')

normalize c@(SimpleBoolContext ns (TApp info t1 t2)) = do
    (SimpleBoolContext ns t1') <- normalize $ c `withTerm` t1
    return $ SimpleBoolContext ns (TApp info t1' t2)

normalize _ = Nothing

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk c (TTrue info) = TTrue info
                  walk c (TFalse info) = TFalse info

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = tmmap onvar c t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar c info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = tmmap onvar 0 t
               where onvar c info name depth | name == j + c = shift c s
                     onvar c info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
