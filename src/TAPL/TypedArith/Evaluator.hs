{-# LANGUAGE FlexibleContexts #-}

module TAPL.TypedArith.Evaluator where

import TAPL.TypedArith.Types
import TAPL.TypedArith.Parser
import TAPL.TypedArith.Context
import TAPL.TypedArith.TypeChecker
import Data.List (findIndex, intercalate, all, nub, (\\), last)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

eval :: String -> String -> Either EvaluationError String
eval code path = do
  case (parse path code) of
    Left e -> Left $ ParsecError e
    Right c@(TypedArithContext ns ast) ->
      if correctAST c
      then case last $ (\t -> fullNormalize $ TypedArithContext ns t) <$> ast of
                result -> case typeOf result of
                               Right ty -> return $ show result ++ ":" ++ show ty
                               Left x -> Left $ TypeError $ show x
      else Left $ TypeError $ show $ head $ typeErrors c

correctAST :: TypedArithContext AST -> Bool
correctAST (TypedArithContext _ []) = False
correctAST c = all isRight $ allTypes c

typeErrors :: TypedArithContext AST -> [TypeError]
typeErrors (TypedArithContext _ []) = []
typeErrors c = map fromLeft $ filter isLeft $ allTypes c

allTypes :: TypedArithContext AST -> [Either TypeError Type]
allTypes (TypedArithContext ns ast) = fmap f ast where f t = typeOf (TypedArithContext ns t)

fullNormalize :: TypedArithContext Term -> TypedArithContext Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: TypedArithContext Term -> Maybe (TypedArithContext Term)
normalize c@(TypedArithContext _ (TIf _ (TTrue _) t _ )) = return $ c `withTerm` t
normalize c@(TypedArithContext _ (TIf _ (TFalse _) _ t)) = return $ c `withTerm` t

normalize c@(TypedArithContext _ (TIf info t1 t2 t3)) = do
  (TypedArithContext _ t1') <- normalize $ c `withTerm` t1
  return $ c `withTerm` (TIf info t1' t2 t3)

normalize c@(TypedArithContext ns (TApp _ (TAbs _ _ _ t) v)) | isVal v =
    return $ TypedArithContext ns (substitutionTop v t)

normalize c@(TypedArithContext ns (TApp info t1 t2)) | isVal t1  = do
    (TypedArithContext ns t2') <- normalize $ c `withTerm` t2
    return $ TypedArithContext ns (TApp info t1 t2')

normalize c@(TypedArithContext ns (TApp info t1 t2)) = do
    (TypedArithContext ns t1') <- normalize $ c `withTerm` t1
    return $ TypedArithContext ns (TApp info t1' t2)

normalize c@(TypedArithContext ns (TSucc info t)) = do
    (TypedArithContext ns t') <- normalize $ c `withTerm` t
    return $ TypedArithContext ns (TSucc info t')

normalize c@(TypedArithContext ns (TPred _ (TZero info))) =
    return $ TypedArithContext ns (TZero info)

normalize c@(TypedArithContext ns (TPred _ (TSucc _ t))) | isNumerical t =
    return $ TypedArithContext ns t

normalize c@(TypedArithContext ns (TPred info t)) = do
    (TypedArithContext ns t') <- normalize $ c `withTerm` t
    return $ TypedArithContext ns (TPred info t')

normalize c@(TypedArithContext ns (TIsZero _ (TZero info))) =
    return $ TypedArithContext ns (TTrue info)

normalize c@(TypedArithContext ns (TIsZero _ (TSucc info t))) | isNumerical t =
    return $ TypedArithContext ns (TFalse info)

normalize c@(TypedArithContext ns (TIsZero info t)) = do
    (TypedArithContext ns t') <- normalize $ c `withTerm` t
    return $ TypedArithContext ns (TIsZero info t')

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

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk c (TTrue info) = TTrue info
                  walk c (TFalse info) = TFalse info
                  walk c (TZero info) = TZero info
                  walk c (TIsZero info t) = TIsZero info (walk c t)
                  walk c (TPred info t) = TPred info (walk c t)
                  walk c (TSucc info t) = TSucc info (walk c t)

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

fromLeft (Right x) = undefined
fromLeft (Left x) = x