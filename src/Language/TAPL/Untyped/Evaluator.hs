{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.Untyped.Evaluator (eval) where

import Language.TAPL.Untyped.Types
import Language.TAPL.Untyped.Parser
import Language.TAPL.Untyped.Context
import Data.List (last)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

eval :: String -> String -> Either EvaluationError String
eval code path = do
  case (parse path code) of
        Right c@(UntypedContext ns ast) -> return $ show $ last  $ (\t -> fullNormalize $ UntypedContext ns t) <$> ast
        Left e -> Left $ ParsecError e

fullNormalize :: UntypedContext Term -> UntypedContext Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: UntypedContext Term -> Maybe (UntypedContext Term)

normalize c@(UntypedContext ns (TApp _ (TAbs _ _ t) v)) | isVal v =
    return $ UntypedContext ns (substitutionTop v t)

normalize c@(UntypedContext ns (TApp info t1 t2)) | isVal t1  = do
    (UntypedContext ns t2') <- normalize $ c `withTerm` t2
    return $ UntypedContext ns (TApp info t1 t2')

normalize c@(UntypedContext ns (TApp info t1 t2)) = do
    (UntypedContext ns t1') <- normalize $ c `withTerm` t1
    return $ UntypedContext ns (TApp info t1' t2)

normalize _ = Nothing

isVal :: Term -> Bool
isVal (TAbs _ _ _) = True
isVal _ = False

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x t) = TAbs info x (walk (c+1) t)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)

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