{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullSimple.Evaluator (evalString) where

import Data.List (findIndex, intercalate, all, nub, (\\), last)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as Map

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

import TAPL.FullSimple.Types
import TAPL.FullSimple.Context
import TAPL.FullSimple.Parser
import TAPL.FullSimple.TypeChecker
import TAPL.FullSimple.Pretty

type Eval a = ReaderT LCNames Maybe a

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
      types <- sequence $ typeOf names <$> ast
      let result = last  $ eval names ast
      ty <- typeOf names result
      result' <- render names result
      return $ result' ++ ":" ++ (show $ pretty ty)

eval :: LCNames -> AST -> AST
eval n ast = fullNormalize n <$> ast

fullNormalize :: LCNames -> Term -> Term
fullNormalize n t =
    case runReaderT (normalize t) n of
         Just t' -> fullNormalize n t'
         Nothing -> t

normalize :: Term -> Eval Term
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

normalize (TRecord _ fields) | (Map.size fields) == 0 = lift Nothing
normalize t@(TRecord _ fields) | isVal t = lift Nothing

normalize (TRecord info fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields
    return $ TRecord info (Map.fromList fields')
    where evalField (k, field) = do
            field' <- normalize field
            return (k, field')

normalize (TLookup _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TLookup _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t

normalize (TLookup _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = lift $ Map.lookup key fields
normalize (TLookup info t k) = do
    t' <- normalize t
    return $ TLookup info t' k

normalize (TLet info v t1 t2) | isVal t1 = return $ substitutionTop t1 t2

normalize (TLet info v t1 t2) = do
    t1' <- normalize t1
    return $ TLet info v t1' t2

normalize (TAscribe info t ty) = return t

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = do
    return $ substitutionTop t1 t2

normalize (TFix info t) = do
    t' <- normalize t
    return $ TFix info t'

normalize (TCase _ (TTag _ key v _) branches) | isVal v = do
    case Map.lookup key branches of
         Just (_, t) -> return $ substitutionTop v t

normalize c@(TCase info t fields) = do
    t' <- normalize t
    return $ TCase info t' fields

normalize _ = lift Nothing

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
                  walk c (TRecord info fields) = TRecord info $ Map.map (walk c) fields
                  walk c (TTag info k t ty) = TTag info k (walk c t) ty
                  walk c (TLookup info r k) = TLookup info (walk c r) k
                  walk c (TLet info x t1 t2) = TLet info x (walk c t1) (walk (c+1) t2)
                  walk c (TAscribe info t ty) = TAscribe info (walk c t) ty
                  walk c (TCase info t branches) = TCase info (walk c t) $ Map.map walkBranch branches
                                             where walkBranch (x, y) = (x, walk (c + 1) y)
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
