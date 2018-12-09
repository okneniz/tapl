{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullRef.Evaluator (eval) where
import TAPL.FullRef.Types
import TAPL.FullRef.Parser
import TAPL.FullRef.Context
import TAPL.FullRef.TypeChecker

import Data.List (all)
import Data.Either (isRight, isLeft)

import Prelude hiding (succ, pred, lookup)
import qualified Prelude (lookup)

eval :: String -> String -> Either EvaluationError String
eval code path = do
    ast <- (wrapErrors $ parse code path)
    let result@(FullRefContext ns mem t) = f ast
    if correctAST ast
    then case typeOf result of
              Right ty -> return $ show result ++ ":" ++ show ty
              Left x -> Left $ TypeError $ show x
    else Left $ TypeError $ show $ head $ typeErrors ast
    where f c@(FullRefContext ns mem [t]) = fullNormalize $ FullRefContext ns mem t
          f c@(FullRefContext ns mem (t:ts)) = f (FullRefContext ns' mem' ts)
            where (FullRefContext ns' mem' _) = fullNormalize $ FullRefContext ns mem t
          wrapErrors (Left e) = Left $ ParsecError e
          wrapErrors (Right x) = Right x

fromLeft (Left x) = x
fromLeft (Right x) = undefined

correctAST :: FullRefContext AST -> Bool
correctAST (FullRefContext _ _ []) = False -- ?
correctAST c = all isRight $ allTypes c -- use isCorrect ?

typeErrors :: FullRefContext AST -> [TypeError]
typeErrors (FullRefContext _ _ []) = []
typeErrors c = map fromLeft $ filter isLeft $ allTypes c

allTypes :: FullRefContext AST -> [Either TypeError Type]
allTypes (FullRefContext ns mem ast) = fmap f ast where f t = typeOf (FullRefContext ns mem t)

fullNormalize :: FullRefContext Term -> FullRefContext Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: FullRefContext Term -> Maybe (FullRefContext Term)
normalize (FullRefContext ns mem (TIf _ (TTrue _) t _)) =
    return $ FullRefContext ns mem t

normalize (FullRefContext ns mem (TIf _ (TFalse _) _ t)) =
    return $ FullRefContext ns mem t

normalize c@(FullRefContext ns mem (TIf info t1 t2 t3)) = do
    (FullRefContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ FullRefContext ns mem' (TIf info t1' t2 t3)

normalize c@(FullRefContext ns mem (TSucc info t)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' (TSucc info t')

normalize c@(FullRefContext ns mem (TPred _ (TZero info))) =
    return $ FullRefContext ns mem (TZero info)

normalize c@(FullRefContext ns mem (TPred _ (TSucc _ t))) | isNumerical $ c `withTerm` t =
    return $ FullRefContext ns mem t

normalize c@(FullRefContext ns mem (TPred info t)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' (TPred info t')

normalize c@(FullRefContext ns mem(TIsZero _ (TZero info))) =
    return $ FullRefContext ns mem (TTrue info)

normalize c@(FullRefContext ns mem(TIsZero _ (TSucc info t))) | isNumerical $ c `withTerm` t =
    return $ FullRefContext ns mem (TFalse info)

normalize c@(FullRefContext ns mem(TIsZero info t)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' (TIsZero info t')

normalize c@(FullRefContext ns mem (TApp _ (TAbs _ _ _ t) v)) | isVal $ c `withTerm` v =
    return $ FullRefContext ns mem (substitutionTop v t)

normalize c@(FullRefContext ns mem (TApp info t1 t2)) | isVal $ c `withTerm` t1  = do
    (FullRefContext ns mem' t2') <- normalize $ c `withTerm` t2
    return $ FullRefContext ns mem' (TApp info t1 t2')

normalize c@(FullRefContext ns mem (TApp info t1 t2)) = do
    (FullRefContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ FullRefContext ns mem' (TApp info t1' t2)

normalize c@(FullRefContext ns mem (TRef info t)) | isVal $ c `withTerm` t = do
    let (location, mem') = extend mem t
    return $ FullRefContext ns mem' (TLoc Nothing location)

normalize c@(FullRefContext ns mem (TRef info t)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' (TRef info t')

normalize c@(FullRefContext ns mem (TDeref info (TLoc _ location))) = do
    return $ c `withTerm` (lookup mem location)

normalize c@(FullRefContext ns mem (TDeref info t)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' (TDeref info t')

normalize c@(FullRefContext ns mem (TAssign info t1 t2)) | not $ isVal (c `withTerm` t1) = do
    (FullRefContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ FullRefContext ns mem' (TAssign info t1' t2)

normalize c@(FullRefContext ns mem (TAssign info t1 t2)) | not $ isVal (c `withTerm` t2) = do
    (FullRefContext ns mem' t2') <- normalize $ c `withTerm` t2
    return $ FullRefContext ns mem' (TAssign info t1 t2')

normalize c@(FullRefContext ns mem (TAssign info (TLoc _ location) t2)) = do
    let mem' = update mem location t2
    return $ FullRefContext ns mem' (TUnit info)

normalize c@(FullRefContext ns mem (TLet info v t1 t2)) | isVal $ c `withTerm` t1 =
    return $ c `withTerm` (substitutionTop t1 t2)

normalize c@(FullRefContext ns mem (TLet info v t1 t2)) = do
    (FullRefContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ FullRefContext ns mem' (TLet info v t1' t2)

normalize c@(FullRefContext ns mem (TAscribe info t ty)) | isVal (c `withTerm` t) = do
    return $ FullRefContext ns mem t

normalize c@(FullRefContext ns mem (TAscribe info t ty)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' t'

normalize c@(FullRefContext ns mem (TPair info t1 t2)) | isVal (c `withTerm` t1)  = do
    (FullRefContext ns mem' t2') <- normalize $ c `withTerm` t2
    return $ FullRefContext ns mem' (TPair info t1 t2')

normalize c@(FullRefContext ns mem (TPair info t1 t2)) = do
    (FullRefContext ns mem' t1') <- normalize $ c `withTerm` t1
    return $ FullRefContext ns mem' (TPair info t1' t2)

normalize c@(FullRefContext ns mem (TRecord _ [])) = Nothing
normalize c@(FullRefContext ns mem t@(TRecord _ fields)) | isVal c = Nothing

normalize c@(FullRefContext ns mem (TRecord info fields)) = do
    return $ FullRefContext ns mem' (TRecord info fields')
     where (mem', fields') = foldl evalField (mem, []) fields
           evalField (m, fs) (k, t) = case normalize $ FullRefContext ns m t of
                                           (Just (FullRefContext ns m' t')) -> evalField (m', fs) (k, t')
                                           _ -> (m, fs ++ [(k, t)])

normalize c@(FullRefContext ns mem (TLookup _ (TPair _ t _) (TInt _ 0))) | isVal $ c `withTerm` t =
    return $ c `withTerm` t

normalize c@(FullRefContext ns mem (TLookup _ (TPair _ _ t) (TInt _ 1))) | isVal $ c `withTerm` t =
    return $ c `withTerm` t

normalize c@(FullRefContext ns mem (TLookup _ t@(TRecord _ fields) (TKeyword _ key))) | isVal $ c `withTerm` t = do
    case Prelude.lookup key fields of
         Just t -> return $ FullRefContext ns mem t
         Nothing -> error $ "Lookup invalid key : " ++ show key ++ " for record " ++ (show $ c `withTerm` t)

normalize c@(FullRefContext ns mem (TLookup info t k)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' (TLookup info t' k)

normalize c@(FullRefContext ns mem t1@(TFix _ a@(TAbs _ _ _ t2))) | isVal $ c `withTerm` a = do
    return $ withTerm c $ substitutionTop t1 t2

normalize c@(FullRefContext ns mem (TFix info t)) = do
    (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
    return $ FullRefContext ns mem' (TFix info t')

normalize c@(FullRefContext ns mem (TCase _ (TTag _ key v _) branches)) | isVal $ c `withTerm` v = do
  (_, t) <- Prelude.lookup key $ (\(key, varName, t) -> (key, (varName, t))) <$> branches
  return $ FullRefContext ns mem $ substitutionTop v t

normalize c@(FullRefContext ns mem (TCase info t fields)) = do
  (FullRefContext ns mem' t') <- normalize $ c `withTerm` t
  return $ FullRefContext ns mem' (TCase info t' fields)

normalize _ = Nothing

isVal :: FullRefContext Term -> Bool
isVal (FullRefContext _ _ (TTrue _)) = True
isVal (FullRefContext _ _ (TFalse _)) = True
isVal (FullRefContext _ _ (TString _ _)) = True
isVal (FullRefContext _ _ (TFloat _ _)) = True
isVal (FullRefContext _ _ (TInt _ _)) = True
isVal (FullRefContext _ _ (TUnit _)) = True
isVal (FullRefContext _ _ (TZero _)) = True
isVal c@(FullRefContext _ _ (TAscribe _ t _)) = isVal $ c `withTerm` t
isVal t | isNumerical t = True
isVal (FullRefContext _ _ (TAbs _ _ _ _)) = True
isVal (FullRefContext _ _ (TLoc _ _)) = True
isVal c@(FullRefContext _ _ (TPair _ t1 t2)) = (isVal $ c `withTerm` t1) && (isVal $ c `withTerm` t2)
isVal c@(FullRefContext ns mem' (TRecord _ fields)) = all (\(_, t) -> isVal $ c `withTerm` t) fields
isVal c@(FullRefContext _ _ (TFix _ t)) = isVal $ c `withTerm` t
isVal c@(FullRefContext _ _ (TTag _ _ t _)) = isVal $ c `withTerm` t
isVal _ = False

isNumerical :: FullRefContext Term -> Bool
isNumerical (FullRefContext _ _ (TZero _)) = True
isNumerical (FullRefContext n s (TSucc _ x)) = isNumerical $ FullRefContext n s x
isNumerical _ = False

termMap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
termMap onvar s t = walk s t
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
                    walk c (TAssign info t1 t2) = TAssign info (walk c t1) (walk c t2)
                    walk c (TRef info t) = TRef info (walk c t)
                    walk c (TDeref info t) = TDeref info (walk c t)
                    walk c (TLet info v t1 t2) = TLet info v (walk c t1) (walk (c + 1) t2)
                    walk c (TAscribe info t ty) = TAscribe info (walk c t) ty
                    walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                    walk c (TRecord info ts) = TRecord info $ (\(k, t) -> (k, walk c t)) <$> ts
                    walk c (TLookup info t1 t2) = TLookup info (walk c t1) (walk c t2)
                    walk c (TTag info k t ty) = TTag info k (walk c t) ty
                    walk c (TCase info t branches) = TCase info (walk c t) $ walkBranch <$> branches
                                               where walkBranch (k, v, x) = (k, v, walk (c + 1) x)
                    walk c t@(TKeyword _ _) = t
                    walk c (TFix info t) = TFix info (walk c t)
                    walk c t@(TLoc _ l) = t
                    walc _ x = error $ show x

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d c t = termMap onvar c t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar c info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = termMap onvar 0 t
               where onvar c info name depth | name == j + c = shift c s
                     onvar c info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
