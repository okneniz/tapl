module Language.TAPL.Recon.Evaluator (evalString) where

import Data.List (last)
import Data.Maybe (fromJust)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

import Language.TAPL.Common.Helpers (whileM)
import Language.TAPL.Common.Context
import Language.TAPL.Recon.Types
import Language.TAPL.Recon.Parser
import Language.TAPL.Recon.Context
import Language.TAPL.Recon.TypeChecker
import Language.TAPL.Recon.Pretty

import Text.Parsec (SourcePos)

evalString :: String -> Either String String
evalString code = do
    case parse "<stdin>" code of
         Left e -> Left $ show e
         Right ([], _) -> return ""
         Right (commands, ns) -> runExcept (evalStateT (f commands) (newState ns))
    where f cs = evalCommands cs >>= \x -> return $ if null x then [] else last x

evalCommands :: [Command] -> Eval [String]
evalCommands [] = return []
evalCommands ((Bind _ name binding):cs) = do
   modify $ \s -> s { names = (bind name binding (names s)) }
   evalCommands cs

evalCommands ((Eval []):cs) = evalCommands cs
evalCommands ((Eval (t:ts)):cs) = do
    ty <- typeOf t
    let t' = fromJust $ whileM normalize t
    (:) <$> render t' ty <*> evalCommands ((Eval ts):cs)

normalize :: Term -> Maybe Term
normalize (TIf _ (TTrue _) t _) = return t
normalize (TIf _ (TFalse _) _ t) = return t
normalize (TIf pos t1 t2 t3) = TIf pos <$> normalize t1 <*> return t2 <*> return t3

normalize (TSucc pos t1) = TSucc pos <$> normalize t1

normalize (TPred _ (TZero pos)) = return $ TZero pos
normalize (TPred _ (TSucc _ t)) | isNumerical  t = return t
normalize (TPred pos t) = TPred pos <$> normalize t

normalize (TIsZero _ (TZero pos)) = return $ TTrue pos
normalize (TIsZero _ (TSucc pos t)) | isNumerical t = return $ TFalse pos
normalize (TIsZero pos t) = TIsZero pos <$> normalize t

normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ termSubstitutionTop v t
normalize (TApp pos t1 t2) | isVal t1 = TApp pos t1 <$> normalize t2
normalize (TApp pos t1 t2) = flip(TApp pos) t2 <$> normalize t1
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

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar pos name depth) = onVar c pos name depth
                           walk c (TAbs pos x ty t1) = TAbs pos x (onType c ty) (walk (c+1) t1)
                           walk c (TApp pos t1 t2) = TApp pos (walk c t1) (walk c t2)
                           walk c (TIf pos t1 t2 t3) = TIf pos (walk c t1) (walk c t2) (walk c t3)
                           walk _ (TTrue pos) = TTrue pos
                           walk _ (TFalse pos) = TFalse pos
                           walk _ (TZero pos) = TZero pos
                           walk c (TIsZero pos t1) = TIsZero pos (walk c t1)
                           walk c (TPred pos t1) = TPred pos (walk c t1)
                           walk c (TSucc pos t1) = TSucc pos (walk c t1)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = termMap onVar (typeShiftAbove d) s t
                 where onVar c pos name depth | name >= c = TVar pos (name + d) (depth + d)
                       onVar _ pos name depth = TVar pos name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType 0 t
                   where onVar c _ name _ | name == j + c = termShift c s
                         onVar _ pos name depth = TVar pos name depth
                         onType _ ty = ty

termSubstitutionTop :: Term -> Term -> Term
termSubstitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
