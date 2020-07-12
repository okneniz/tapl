module Language.TAPL.Equirec.Evaluator (evalString) where

import Language.TAPL.Equirec.Types
import Language.TAPL.Equirec.Parser
import Language.TAPL.Equirec.TypeChecker
import Language.TAPL.Equirec.Pretty

import Text.Parsec (SourcePos)

evalString :: String -> String -> Either String String
evalString code source = do
  case parse source code of
    Left e -> Left $ show e
    Right (ast, names) -> do
      _ <- sequence $ typeOf names <$> ast
      let result = last $ eval ast
      ty <- typeOf names result
      result' <- render names result
      resultType <- renderType names ty
      return $ result' ++ ":" ++ resultType

eval :: AST -> AST
eval ast = fullNormalize <$> ast

fullNormalize :: Term -> Term
fullNormalize t =
    case normalize t of
         Just t' -> fullNormalize t'
         Nothing -> t

normalize :: Term -> Maybe Term
normalize (TApp _ (TAbs _ _ _ t) v) | isVal v = return $ substitutionTop v t
normalize (TApp info t1 t2) | isVal t1 = TApp info t1 <$> normalize t2
normalize (TApp info t1 t2) = normalize t1 >>= \t1' -> return $ TApp info t1' t2
normalize _ = Nothing

isVal :: Term -> Bool
isVal (TAbs _ _ _ _) = True
isVal _ = False

termMap :: (Int -> SourcePos -> VarName -> Depth -> Term) -> (Int -> Type -> Type) -> Int -> Term -> Term
termMap onVar onType s t = walk s t
                     where walk c (TVar info name depth) = onVar c info name depth
                           walk c (TAbs info x ty t1) = TAbs info x (onType c ty) (walk (c+1) t1)
                           walk c (TApp info t1 t2) = TApp info (walk c t1) (walk c t2)

termShiftAbove :: Depth -> VarName -> Term -> Term
termShiftAbove d s t = termMap onVar (typeShiftAbove d) s t
                 where onVar c info name depth | name >= c = TVar info (name + d) (depth + d)
                       onVar _ info name depth = TVar info name (depth + d)

termShift :: VarName -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubstitution :: VarName -> Term -> Term -> Term
termSubstitution j s t = termMap onVar onType 0 t
                   where onVar c _ name _ | name == j + c = termShift c s
                         onVar _ info name depth = TVar info name depth
                         onType _ ty = ty

substitutionTop :: Term -> Term -> Term
substitutionTop s t = termShift (-1) (termSubstitution 0 (termShift 1 s) t)
