module Language.TAPL.FullRef.Evaluator (evalString) where

import Data.List (last)
import qualified Data.Map.Strict as Map

import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)

import Language.TAPL.FullRef.Types
import Language.TAPL.FullRef.Parser
import Language.TAPL.FullRef.TypeChecker
import Language.TAPL.FullRef.Pretty

type Eval a = StateT LCMemory Maybe a

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right ([], _) -> return ""
    Right (ast, names) -> do
        let mem = [] :: LCMemory
        _ <- sequence $ typeOf names mem <$> ast
        let (ast', mem') = eval mem ast
            result = last $ ast'
        ty <- typeOf names mem' result
        result' <- render names result
        return $ result' ++ ":" ++ (show $ pretty ty)

eval :: LCMemory -> AST -> (AST, LCMemory)
eval mem ast = f [] mem ast
    where f acc m [] = (reverse acc, m)
          f acc m (t:ts) = f (t':acc) m' ts where (t', m') = fullNormalize m t

fullNormalize :: LCMemory -> Term -> (Term, LCMemory)
fullNormalize m t =
    case runStateT (normalize t) m of
         Just (t', m') -> fullNormalize m' t'
         Nothing -> (t, m)

normalize :: Term -> Eval Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t

normalize (TIf info t1 t2 t3) = do
    t1' <- normalize t1
    return $ TIf info t1' t2 t3

normalize (TSucc info t1) = do
    t' <- normalize t1
    return $ TSucc info t'

normalize (TPred _ (TZero info)) = return $ TZero info
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t

normalize (TPred info t) = do
    t' <- normalize t
    return $ TPred info t'

normalize (TIsZero _ (TZero info)) = return $ TTrue info
normalize (TIsZero _ (TSucc info t)) | isNumerical t = return $ TFalse info

normalize (TIsZero info t) = do
    t' <- normalize t
    return $ TIsZero info t'

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t

normalize (TApp info t1 t2) | isVal t1  = do
    t2' <- normalize t2
    return $ TApp info t1 t2'

normalize (TApp info t1 t2) = do
    t1' <- normalize t1
    return $ TApp info t1' t2

normalize (TRef info t) | isVal t = do
    mem <- get
    let (location, mem') = Language.TAPL.FullRef.Types.extend mem t
    put mem'
    return $ TLoc info location

normalize (TRef info t) = do
    t' <- normalize t
    return $ TRef info t'

normalize (TDeref _ (TLoc _ location)) = do
    mem <- get
    return $ Language.TAPL.FullRef.Types.lookup mem location

normalize (TDeref info t) = do
    t' <- normalize t
    return $ TDeref info t'

normalize (TAssign info t1 t2) | not (isVal t1) = do
    t1' <- normalize t1
    return $ TAssign info t1' t2

normalize (TAssign info t1 t2) | not $ isVal t2 = do
    t2' <- normalize t2
    return $ TAssign info t1 t2'

normalize (TAssign info (TLoc _ location) t2) = do
    mem <- get
    put $ update mem location t2
    return $ TUnit info

normalize (TLet _ _ t1 t2) | isVal t1 =
    return $ substitutionTop t1 t2

normalize (TLet info v t1 t2) = do
    t1' <- normalize t1
    return $ TLet info v t1' t2

normalize (TAscribe _ t _) | isVal t = return t
normalize (TAscribe _ t _) = normalize t

normalize (TPair info t1 t2) | isVal t1  = do
    t2' <- normalize t2
    return $ TPair info t1 t2'

normalize (TPair info t1 t2) = do
    t1' <- normalize t1
    return $ TPair info t1' t2

normalize (TRecord _ fields) | (Map.size fields) == 0 = lift Nothing
normalize t@(TRecord _ _) | isVal t = lift Nothing

normalize (TRecord info fields) = do
    fields' <- sequence $ evalField <$> Map.toList fields -- не то!
    return $ TRecord info (Map.fromList fields')
    where evalField (k, field) | isVal field = return (k, field)
          evalField (k, field) = do
            field' <- normalize field
            return (k, field')

normalize (TLookup _ t@(TRecord _ fields) (TKeyword _ key)) | isVal t = lift $ Map.lookup key fields

normalize (TLookup _ (TPair _ t _) (TInt _ 0)) | isVal t = return t
normalize (TLookup _ (TPair _ _ t) (TInt _ 1)) | isVal t = return t

normalize (TLookup _ (TRecord _ fields) (TKeyword _ key)) = lift $ Map.lookup key fields

normalize (TLookup info t k) = do
    t' <- normalize t
    return $ TLookup info t' k

normalize t1@(TFix _ a@(TAbs _ _ _ t2)) | isVal a = do
    return $ substitutionTop t1 t2

normalize (TFix info t) = do
    t' <- normalize t
    return $ TFix info t'

normalize (TCase _ (TTag _ key v _) branches) | isVal v = do
    case Map.lookup key branches of
         Just (_, t) -> return $ substitutionTop v t
         Nothing -> lift Nothing

normalize (TCase info t fields) = do
    t' <- normalize t
    return $ TCase info t' fields

normalize _ = lift Nothing

isVal :: Term -> Bool
isVal (TTrue _) = True
isVal (TFalse _) = True
isVal (TString _ _) = True
isVal (TFloat _ _) = True
isVal (TInt _ _) = True
isVal (TUnit _) = True
isVal (TZero _) = True
isVal (TAscribe _ t _) = isVal t
isVal t | isNumerical t = True
isVal (TAbs _ _ _ _) = True
isVal (TLoc _ _) = True
isVal (TPair _ t1 t2) = (isVal t1) && (isVal t2)
isVal (TRecord _ ts) = all isVal $ Map.elems ts
isVal (TFix _ t) = isVal t
isVal (TTag _ _ t _) = isVal t
isVal _ = False

isNumerical :: Term -> Bool
isNumerical (TZero _) = True
isNumerical (TSucc _ t) = isNumerical t
isNumerical _ = False

termMap :: (Int -> Info -> Depth -> VarName -> Term) -> Int -> Term -> Term
termMap onvar s t = walk s t
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
                    walk c (TAssign info t1 t2) = TAssign info (walk c t1) (walk c t2)
                    walk c (TRef info t1) = TRef info (walk c t1)
                    walk c (TDeref info t1) = TDeref info (walk c t1)
                    walk c (TLet info v t1 t2) = TLet info v (walk c t1) (walk (c + 1) t2)
                    walk c (TAscribe info t1 ty) = TAscribe info (walk c t1) ty
                    walk c (TPair info t1 t2) = TPair info (walk c t1) (walk c t2)
                    walk c (TRecord info fields) = TRecord info $ Map.map (walk c) fields
                    walk c (TLookup info t1 t2) = TLookup info (walk c t1) (walk c t2)
                    walk c (TTag info k t1 ty) = TTag info k (walk c t1) ty
                    walk c (TCase info t1 branches) = TCase info (walk c t1) $ Map.map walkBranch branches
                                                where walkBranch (x, y) = (x, walk (c + 1) y)
                    walk _ t1@(TKeyword _ _) = t1
                    walk c (TFix info t1) = TFix info (walk c t1)
                    walk _ t1@(TLoc _ _) = t1

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = termMap onvar s t
                 where onvar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onvar _ info name depth = TVar info name (depth + d)

shift :: VarName -> Term -> Term
shift d t = termShiftAbove d 0 t

substitution :: VarName -> Term -> Term -> Term
substitution j s t = termMap onvar 0 t
               where onvar c _ name _ | name == j + c = shift c s
                     onvar _ info name depth = TVar info name depth

substitutionTop :: Term -> Term -> Term
substitutionTop s t = shift (-1) (substitution 0 (shift 1 s) t)
