{-# LANGUAGE FlexibleContexts #-}

module TAPL.Equirec.Evaluator (eval) where
import TAPL.Equirec.Types
import TAPL.Equirec.Parser
import TAPL.Equirec.Context
import TAPL.Equirec.TypeChecker

import Data.List (all)
import Data.Either (isRight, isLeft)

import Prelude hiding (succ, pred, lookup)
import qualified Prelude (lookup)

eval :: String -> String -> Either EvaluationError String
eval code path = do
    ast <- (wrapErrors $ parse code path)
    let result@(EquirecContext ns t) = f ast
    if correctAST ast
    then case typeOf result of
              Right ty -> return $ show result ++ ":" ++ (show $ EquirecContext ns ty)
              Left x -> Left $ TypeError $ show x
    else Left $ TypeError $ show $ head $ typeErrors ast
    where f c@(EquirecContext ns [t]) = fullNormalize $ EquirecContext ns t
          f c@(EquirecContext ns (t:ts)) = f (EquirecContext ns' ts)
            where (EquirecContext ns' _) = fullNormalize $ EquirecContext ns t
          wrapErrors (Left e) = Left $ ParsecError e
          wrapErrors (Right x) = Right x

fromLeft (Left x) = x
fromLeft (Right x) = undefined

correctAST :: EquirecContext AST -> Bool
correctAST (EquirecContext _ []) = False -- ?
correctAST c = all isRight $ allTypes c -- use isCorrect ?

typeErrors :: EquirecContext AST -> [TypeError]
typeErrors (EquirecContext _ []) = []
typeErrors c = map fromLeft $ filter isLeft $ allTypes c

allTypes :: EquirecContext AST -> [Either TypeError Type]
allTypes (EquirecContext ns ast) = fmap f ast where f t = typeOf (EquirecContext ns t)

fullNormalize :: EquirecContext Term -> EquirecContext Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: EquirecContext Term -> Maybe (EquirecContext Term)
normalize c@(EquirecContext ns (TApp _ (TAbs _ _ _ t) v)) | isVal $ (EquirecContext ns v) =
    return $ EquirecContext ns (substitutionTop v t)

normalize c@(EquirecContext ns (TApp info t1 t2)) | isVal $ (EquirecContext ns t1)  = do
    (EquirecContext ns' t2') <- normalize $ (EquirecContext ns t2)
    return $ EquirecContext ns' (TApp info t1 t2')

normalize c@(EquirecContext ns (TApp info t1 t2)) = do
    (EquirecContext ns' t1') <- normalize $ (EquirecContext ns t1)
    return $ EquirecContext ns' (TApp info t1' t2)

normalize _ = Nothing

isVal :: EquirecContext Term -> Bool
isVal (EquirecContext _ (TAbs _ _ _ _)) = True
isVal _ = False

termMap :: (Int -> Info -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar info name depth) = onVar c info name depth
                           walk c (TAbs info x ty t) = TAbs info x (onType c ty) (walk (c+1) t)
                           walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                           walc _ x = error $ show x

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onVar (typeShiftAbove d) c t
                 where onVar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onVar c info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = termMap onVar onType 0 t
               where onVar c info name depth | name == j + c = shift c s
                     onVar c info name depth = TVar info name depth
                     onType j ty = ty

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
