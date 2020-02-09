module Language.TAPL.FullSimple.Evaluator (evalString) where

import Data.List (all, last)
import qualified Data.Map.Strict as Map

import Control.Monad (liftM)

import Language.TAPL.FullSimple.Types
import Language.TAPL.FullSimple.Parser
import Language.TAPL.FullSimple.TypeChecker
import Language.TAPL.FullSimple.Pretty

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
      _ <- sequence $ typeOf names <$> ast
      let result = last  $ eval ast
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
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t

normalize (TIf info t1 t2 t3) = do
    t1' <- normalize t1
    return $ TIf info t1' t2 t3

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t

normalize (TApp info t1 t2) | isVal t1 = do
    t2' <- normalize t2
    return $ TApp info t1 t2'

normalize (TApp info t1 t2) = do
    t1' <- normalize t1
    return $ TApp info t1' t2

normalize (TSucc info t) = do
    t' <- normalize t
    return $ TSucc info t'

normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical t = return t
normalize (TPred info t) = do
    t' <- normalize t
    return $ TPred info t'

normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t = return $ TFalse info
normalize (TIsZero info t) = do
    t' <- normalize t
    return $ TIsZero info t'

normalize (TPair info t1 t2) | isVal t1 = do
    t2' <- normalize t2
    return $ TPair info t1 t2'

normalize (TPair info t1 t2) = do
    t1' <- normalize t1
    return $ TPair info t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = Nothing

normalize (TLookup _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TLookup _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t
normalize (TLookup _ (TRecord _ fields) (TKeyword _ key)) = Map.lookup key fields

normalize (TLookup info t k) = do
    t' <- normalize t
    return $ TLookup info t' k

normalize (TLet _ _ t1 t2) | isVal t1 = return $ substitutionTop t1 t2

normalize (TLet info v t1 t2) = do
    t1' <- normalize t1
    return $ TLet info v t1' t2

normalize (TAscribe _ t _) = return t

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = do
    return $ substitutionTop t1 t2

normalize (TFix info t) = do
    t' <- normalize t
    return $ TFix info t'

normalize (TCase _ (TTag _ key v _) branches) | isVal v =
    liftM (\(_, t) -> substitutionTop v t)
          (Map.lookup key branches)

normalize (TCase info t fields) = do
    t' <- normalize t
    return $ TCase info t' fields

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
isVal (TRecord _ ts) = all isVal $ Map.elems ts
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
                  walk c (TAbs info x ty t1) = TAbs info x ty (walk (c+1) t1)
                  walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)
                  walk c (TIf info t1 t2 t3) = TIf info (walk c t1) (walk c t2) (walk c t3)
                  walk _ (TTrue info) = TTrue info
                  walk _ (TFalse info) = TFalse info
                  walk _ (TString info x) = TString info x
                  walk _ (TUnit info) = TUnit info
                  walk _ (TZero info) = TZero info
                  walk c (TIsZero info t1) = TIsZero info (walk c t1)
                  walk c (TPred info t1) = TPred info (walk c t1)
                  walk c (TSucc info t1) = TSucc info (walk c t1)
                  walk _ (TFloat info t1) = TFloat info t1
                  walk _ (TInt info t1) = TInt info t1
                  walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                  walk c (TRecord info fields) = TRecord info $ Map.map (walk c) fields
                  walk c (TTag info k t1 ty) = TTag info k (walk c t1) ty
                  walk c (TLookup info r k) = TLookup info (walk c r) k
                  walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)
                  walk c (TAscribe info t1 ty) = TAscribe info (walk c t1) ty
                  walk c (TCase info t1 branches) = TCase info (walk c t1) $ Map.map walkBranch branches
                                              where walkBranch (x, y) = (x, walk (c + 1) y)
                  walk c (TFix info t1) = TFix info (walk c t1)
                  walk _ t1@(TKeyword _ _) = t1

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
