{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullError.Evaluator where

import Control.Monad (liftM)

import TAPL.FullError.Types
import TAPL.FullError.Parser
import Data.List (all, intercalate)
import Text.Parsec.Error (ParseError(..))
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)
import Data.Either (isRight, isLeft)
import TAPL.FullError.Memory

import Prelude hiding (succ, pred, lookup)
import qualified Prelude (lookup)

evalFile :: String -> IO (Either EvaluationError String)
evalFile path = do
    code <- readFile path
    return $ eval code path

eval :: String -> String -> Either EvaluationError String
eval code path = do
    ast <- (wrapErrors $ parse code path)
    let result@(LCParserContext ns mem t) = f ast
    if correctAST ast
    then case typeOf result of
              Right ty -> return $ show result ++ ":" ++ show ty
              Left x -> Left $ TypeError $ show x
    else Left $ TypeError $ show $ head $ typeErrors ast
    where f c@(LCParserContext ns mem [t]) = fullNormalize $ LCParserContext ns mem t
          f c@(LCParserContext ns mem (t:ts)) = f (LCParserContext ns' mem' ts)
            where (LCParserContext ns' mem' _) = fullNormalize $ LCParserContext ns mem t
          wrapErrors (Left e) = Left $ ParsecError e
          wrapErrors (Right x) = Right x

fromRight (Right x) = x
fromRight (Left x) = undefined

fromLeft (Left x) = x
fromLeft (Right x) = undefined

correctAST :: LCParserContext AST -> Bool
correctAST (LCParserContext _ _ []) = False
correctAST c = all isRight $ allTypes c

typeErrors :: LCParserContext AST -> [TypeError]
typeErrors (LCParserContext _ _ []) = []
typeErrors c = map fromLeft $ filter isLeft $ allTypes c

help :: LCParserContext AST -> AST
help (LCParserContext ns mem ast) = ast

allTypes :: LCParserContext AST -> [Either TypeError Type]
allTypes (LCParserContext ns mem ast) = fmap f ast
    where f t = typeOf (LCParserContext ns mem t)

fullNormalize :: LCParserContext Term -> LCParserContext Term
fullNormalize c = case normalize c of
                       Just c' ->
                        fullNormalize c'
                       Nothing -> c

normalize :: LCParserContext Term -> Maybe (LCParserContext Term)
normalize (LCParserContext ns mem (TIf _ (TTrue _) t _)) =
    return $ LCParserContext ns mem t

normalize (LCParserContext ns mem (TIf _ (TFalse _) _ t)) =
    return $ LCParserContext ns mem t

-- без подтипов не имеет смысла
--normalize (LCParserContext ns mem (TIf _ e@(TError _) t _)) =
--    return $ LCParserContext ns mem e

normalize c@(LCParserContext ns mem (TIf info t1 t2 t3)) = do
    (LCParserContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ LCParserContext ns mem' (TIf info t1' t2 t3)

normalize c@(LCParserContext ns mem (TSucc info t)) = do
    (LCParserContext ns mem' t') <- normalize $ c `withTerm` t
    return $ LCParserContext ns mem' (TSucc info t')

normalize c@(LCParserContext ns mem (TPred _ (TZero info))) =
    return $ LCParserContext ns mem (TZero info)

normalize c@(LCParserContext ns mem (TPred _ (TSucc _ t))) | isNumerical $ c `withTerm` t =
    return $ LCParserContext ns mem t

normalize c@(LCParserContext ns mem (TPred info t)) = do
    (LCParserContext ns mem' t') <- normalize $ c `withTerm` t
    return $ LCParserContext ns mem' (TPred info t')

normalize c@(LCParserContext ns mem(TIsZero _ (TZero info))) =
    return $ LCParserContext ns mem (TTrue info)

normalize c@(LCParserContext ns mem(TIsZero _ (TSucc info t))) | isNumerical $ c `withTerm` t =
    return $ LCParserContext ns mem (TFalse info)

normalize c@(LCParserContext ns mem(TIsZero info t)) = do
    (LCParserContext ns mem' t') <- normalize $ c `withTerm` t
    return $ LCParserContext ns mem' (TIsZero info t')

normalize c@(LCParserContext ns mem (TApp _ t1@(TError _) t2))  =
    return $ LCParserContext ns mem t1

normalize c@(LCParserContext ns mem (TApp _ t1 t2@(TError _))) | isVal $ c `withTerm` t1 =
    return $ LCParserContext ns mem t2

normalize c@(LCParserContext ns mem (TApp _ (TAbs _ _ _ t) v)) | isVal $ c `withTerm` v =
    return $ LCParserContext ns mem (substitutionTop v t)

normalize c@(LCParserContext ns mem (TApp info t1 t2)) | isVal $ c `withTerm` t1  = do
    (LCParserContext ns mem' t2') <- normalize $ c `withTerm` t2
    return $ LCParserContext ns mem' (TApp info t1 t2')

normalize c@(LCParserContext ns mem (TApp info t1 t2)) = do
    (LCParserContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ LCParserContext ns mem' (TApp info t1' t2)

normalize c@(LCParserContext ns mem (TRef info t)) | isVal $ c `withTerm` t = do
    let (location, mem') = extend mem t
    return $ LCParserContext ns mem' (TLoc Nothing location)

normalize c@(LCParserContext ns mem (TRef info t)) = do
    (LCParserContext ns mem' t') <- normalize $ c `withTerm` t
    return $ LCParserContext ns mem' (TRef info t')

normalize c@(LCParserContext ns mem (TDeref info (TLoc _ location))) = do
    return $ c `withTerm` (lookup mem location)

normalize c@(LCParserContext ns mem (TDeref info t)) = do
    (LCParserContext ns mem' t') <- normalize $ c `withTerm` t
    return $ LCParserContext ns mem' (TDeref info t')

normalize c@(LCParserContext ns mem (TAssign info t1 t2)) | not $ isVal (c `withTerm` t1) = do
    (LCParserContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ LCParserContext ns mem' (TAssign info t1' t2)

normalize c@(LCParserContext ns mem (TAssign info t1 t2)) | not $ isVal (c `withTerm` t2) = do
    (LCParserContext ns mem' t2') <- normalize $ c `withTerm` t2
    return $ LCParserContext ns mem' (TAssign info t1 t2')

normalize c@(LCParserContext ns mem (TAssign info (TLoc _ location) t2)) = do
    let mem' = update mem location t2
    return $ LCParserContext ns mem' (TUnit info)

normalize c@(LCParserContext ns mem (TLet info v t1 t2)) | isVal $ c `withTerm` t1 =
    return $ c `withTerm` (substitutionTop t1 t2)

normalize c@(LCParserContext ns mem (TLet info v t1 t2)) = do
    (LCParserContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ LCParserContext ns mem' (TLet info v t1' t2)

normalize _ = Nothing

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk c (TTrue info) = TTrue info
                  walk c (TFalse info) = TFalse info
                  walk c (TString info s) = TString info s
                  walk c (TUnit info) = TUnit info
                  walk c (TZero info) = TZero info
                  walk c (TError info) = TError info
                  walk c (TIsZero info t) = TIsZero info (walk c t)
                  walk c (TPred info t) = TPred info (walk c t)
                  walk c (TSucc info t) = TSucc info (walk c t)
                  walk c (TFloat info t) = TFloat info t
                  walk c (TAssign info t1 t2) = TAssign info (walk c t1) (walk c t2)
                  walk c (TRef info t) = TRef info (walk c t)
                  walk c (TDeref info t) = TDeref info (walk c t)
                  walk c (TLet info v t1 t2) = TLet info v (walk c t1) (walk (c + 1) t2)
                  walk c t@(TLoc _ _) = t

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