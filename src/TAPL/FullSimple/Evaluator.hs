{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullSimple.Evaluator (eval) where

import TAPL.FullSimple.Types
import TAPL.FullSimple.Parser
import TAPL.FullSimple.Context
import TAPL.FullSimple.TypeChecker
import Data.List (findIndex, intercalate, all, nub, (\\), last)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)

eval :: String -> String -> Either EvaluationError String
eval code path = do
  case (parse path code) of
    Left e -> Left $ ParsecError e
    Right c@(FullSimpleContext ns ast) ->
      if correctAST c
      then case last $ (\t -> fullNormalize $ FullSimpleContext ns t) <$> ast of
                result -> case typeOf result of
                               Right ty -> return $ show result ++ ":" ++ show ty
                               Left x -> Left $ TypeError $ show x
      else Left $ TypeError $ show $ head $ typeErrors c

correctAST :: FullSimpleContext AST -> Bool
correctAST (FullSimpleContext _ []) = False -- ?
correctAST c = all isRight $ allTypes c -- use isCorrect ?

typeErrors :: FullSimpleContext AST -> [TypeError]
typeErrors (FullSimpleContext _ []) = []
typeErrors c = map fromLeft $ filter isLeft $ allTypes c

allTypes :: FullSimpleContext AST -> [Either TypeError Type]
allTypes (FullSimpleContext ns ast) = fmap f ast where f t = typeOf (FullSimpleContext ns t)

fullNormalize :: FullSimpleContext Term -> FullSimpleContext Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: FullSimpleContext Term -> Maybe (FullSimpleContext Term)
normalize c@(FullSimpleContext _ (TIf _ (TTrue _) t _ )) = return $ c `withTerm` t
normalize c@(FullSimpleContext _ (TIf _ (TFalse _) _ t)) = return $ c `withTerm` t

normalize c@(FullSimpleContext _ (TIf info t1 t2 t3)) = do
  (FullSimpleContext _ t1') <- normalize $ c `withTerm` t1
  return $ c `withTerm` (TIf info t1' t2 t3)

normalize c@(FullSimpleContext ns (TApp _ (TAbs _ _ _ t) v)) | isVal v =
    return $ FullSimpleContext ns (substitutionTop v t)

normalize c@(FullSimpleContext ns (TApp info t1 t2)) | isVal t1  = do
    (FullSimpleContext ns t2') <- normalize $ c `withTerm` t2
    return $ FullSimpleContext ns (TApp info t1 t2')

normalize c@(FullSimpleContext ns (TApp info t1 t2)) = do
    (FullSimpleContext ns t1') <- normalize $ c `withTerm` t1
    return $ FullSimpleContext ns (TApp info t1' t2)

normalize c@(FullSimpleContext ns (TSucc info t)) = do
    (FullSimpleContext ns t') <- normalize $ c `withTerm` t
    return $ FullSimpleContext ns (TSucc info t')

normalize c@(FullSimpleContext ns (TPred _ (TZero info))) =
    return $ FullSimpleContext ns (TZero info)

normalize c@(FullSimpleContext ns (TPred _ (TSucc _ t))) | isNumerical t =
    return $ FullSimpleContext ns t

normalize c@(FullSimpleContext ns (TPred info t)) = do
    (FullSimpleContext ns t') <- normalize $ c `withTerm` t
    return $ FullSimpleContext ns (TPred info t')

normalize c@(FullSimpleContext ns (TIsZero _ (TZero info))) =
    return $ FullSimpleContext ns (TTrue info)

normalize c@(FullSimpleContext ns (TIsZero _ (TSucc info t))) | isNumerical t =
    return $ FullSimpleContext ns (TFalse info)

normalize c@(FullSimpleContext ns (TIsZero info t)) = do
    (FullSimpleContext ns t') <- normalize $ c `withTerm` t
    return $ FullSimpleContext ns (TIsZero info t')

normalize c@(FullSimpleContext ns (TPair info t1 t2)) | isVal t1  = do
    (FullSimpleContext ns t2') <- normalize $ c `withTerm` t2
    return $ FullSimpleContext ns (TPair info t1 t2')

normalize c@(FullSimpleContext ns (TPair info t1 t2)) = do
    (FullSimpleContext ns t1') <- normalize $ c `withTerm` t1
    return $ FullSimpleContext ns (TPair info t1' t2)

normalize (FullSimpleContext ns (TRecord _ [])) = Nothing
normalize (FullSimpleContext ns t@(TRecord _ fields)) | isVal t = Nothing

normalize c@(FullSimpleContext ns (TRecord info fields)) = do
    return $ FullSimpleContext ns (TRecord info fields')
     where (fields') = foldl evalField [] fields
           evalField fs (k, t) = case normalize $ FullSimpleContext ns t of
                                      (Just (FullSimpleContext ns t')) -> evalField fs (k, t')
                                      _ -> fs ++ [(k, t)]

normalize c@(FullSimpleContext ns (TLookup _ (TPair _ t _) (TInt _ 0))) | isVal t = return $ c `withTerm` t
normalize c@(FullSimpleContext ns (TLookup _ (TPair _ _ t) (TInt _ 1))) | isVal t = return $ c `withTerm` t

normalize c@(FullSimpleContext ns (TLookup _ t@(TRecord _ fields) (TKeyword _ key))) | isVal t = do
    case Prelude.lookup key fields of
         Just t -> return $ FullSimpleContext ns t
         Nothing -> error $ "Lookup invalid key : " ++ show key ++ " for record " ++ (show $ c `withTerm` t)

normalize c@(FullSimpleContext ns (TLookup info t k)) = do
    (FullSimpleContext ns t') <- normalize $ c `withTerm` t
    return $ FullSimpleContext ns (TLookup info t' k)

normalize c@(FullSimpleContext ns (TLet info v t1 t2)) | isVal t1 =
    return $ c `withTerm` (substitutionTop t1 t2)

normalize c@(FullSimpleContext ns (TLet info v t1 t2)) = do
    (FullSimpleContext ns t1') <- normalize $ c `withTerm` t1
    return $ FullSimpleContext ns (TLet info v t1' t2)

normalize c@(FullSimpleContext ns (TAscribe info t ty)) | isVal t = do
    return $ FullSimpleContext ns t

normalize c@(FullSimpleContext ns (TAscribe info t ty)) = do
    (FullSimpleContext ns t') <- normalize $ c `withTerm` t
    return $ FullSimpleContext ns t'

normalize c@(FullSimpleContext ns t1@(TFix _ a@(TAbs _ _ _ t2))) | isVal a = do
    return $ withTerm c $ substitutionTop t1 t2

normalize c@(FullSimpleContext ns (TFix info t)) = do
    (FullSimpleContext ns t') <- normalize $ c `withTerm` t
    return $ FullSimpleContext ns (TFix info t')

normalize (FullSimpleContext ns (TCase _ (TTag _ key v _) branches)) | isVal v = do
  (_, t) <- lookup key $ (\(key, varName, t) -> (key, (varName, t))) <$> branches
  return $ FullSimpleContext ns $ substitutionTop v t

normalize c@(FullSimpleContext ns (TCase info t fields)) = do
  (FullSimpleContext ns t') <- normalize $ c `withTerm` t
  return $ FullSimpleContext ns (TCase info t' fields)

normalize _ = Nothing

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TAbs _ _ _ _) = True
isVal (TString _ _) = True
isVal (TUnit _) = True
isVal (TFloat _ _) = True
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal x | isNumerical x = True
isVal (TRecord _ ts) = all (\(_,y) -> isVal y) ts
isVal (TAscribe _ t _) = isVal t
isVal (TTag _ _ t _) = isVal t
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
                  walk c (TString info s) = TString info s
                  walk c (TUnit info) = TUnit info
                  walk c (TZero info) = TZero info
                  walk c (TIsZero info t) = TIsZero info (walk c t)
                  walk c (TPred info t) = TPred info (walk c t)
                  walk c (TSucc info t) = TSucc info (walk c t)
                  walk c (TFloat info t) = TFloat info t
                  walk c (TInt info t) = TInt info t
                  walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                  walk c (TRecord info fields) = TRecord info $ (\(k,v) -> (k, walk c v)) <$> fields
                  walk c (TTag info k t ty) = TTag info k (walk c t) ty
                  walk c (TLookup info r k) = TLookup info (walk c r) k
                  walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)
                  walk c (TAscribe info t ty) = TAscribe info (walk c t) ty
                  walk c (TCase info t branches) = TCase info (walk c t) $ walkBranch <$> branches
                                             where walkBranch (k, v, x) = (k, v, walk (c + 1) x)
                  walk c (TFix info t) = TFix info (walk c t)
                  walk c t@(TKeyword _ _) = t

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
