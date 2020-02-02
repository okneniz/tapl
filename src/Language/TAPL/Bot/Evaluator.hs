{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.Bot.Evaluator where

import Control.Monad (liftM)

import Language.TAPL.Bot.Types
import Language.TAPL.Bot.Parser
import Data.List (all, intercalate)
import Text.Parsec.Error (ParseError(..))
import Text.Parsec.Pos (sourceLine, sourceColumn, sourceName)
import Data.Either (isRight, isLeft)
import Prelude hiding (lookup)
import qualified Prelude (lookup)

eval :: String -> String -> Either EvaluationError String
eval code path = do
    ast <- (wrapErrors $ parse code path)
    let result@(LCParserContext ns (Just t)) = f ast
    if correctAST ast
    then case typeOf result of
              Right ty -> return $ show result ++ ":" ++ show ty
              Left x -> Left $ TypeError $ show x
    else Left $ TypeError $ show $ head $ typeErrors ast
    where f c@(LCParserContext ns (Just [t])) = fullNormalize $ LCParserContext ns $ Just t
          f c@(LCParserContext ns (Just (t:ts))) = f (LCParserContext ns' $ Just ts)
            where (LCParserContext ns' _) = fullNormalize $ LCParserContext ns $ Just t
          wrapErrors (Left e) = Left $ ParsecError e
          wrapErrors (Right x) = Right x

fromLeft (Left x) = x
fromLeft (Right x) = undefined

correctAST :: LCParserContext AST -> Bool
correctAST (LCParserContext _ (Just [])) = False
correctAST c = all isRight $ allTypes c

typeErrors :: LCParserContext AST -> [TypeError]
typeErrors (LCParserContext _ (Just [])) = []
typeErrors c = map fromLeft $ filter isLeft $ allTypes c

help :: LCParserContext AST -> AST
help (LCParserContext ns (Just ast)) = ast

allTypes :: LCParserContext AST -> [Either TypeError Type]
allTypes (LCParserContext ns (Just ast)) = fmap f ast
    where f t = typeOf (LCParserContext ns (Just t))

fullNormalize :: LCParserContext Term -> LCParserContext Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: LCParserContext Term -> Maybe (LCParserContext Term)
normalize c@(LCParserContext ns (Just (TApp _ (TAbs _ _ _ t) v))) | isVal $ c `withTerm` v =
    return $ LCParserContext ns $ Just (substitutionTop v t)

normalize c@(LCParserContext ns (Just (TApp info t1 t2))) | isVal $ c `withTerm` t1  = do
    (LCParserContext ns (Just t2')) <- normalize $ c `withTerm` t2
    return $ LCParserContext ns $ Just (TApp info t1 t2')

normalize c@(LCParserContext ns (Just (TApp info t1 t2))) = do
    (LCParserContext ns (Just t1')) <- normalize $ c `withTerm` t1
    return $ LCParserContext ns $ Just (TApp info t1' t2)

normalize _ = Nothing

tmmap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
tmmap onvar s t = walk s t
            where walk c (TVar info name depth) = onvar c info name depth
                  walk c (TAbs info x ty t) = TAbs info x ty (walk (c+1) t)
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