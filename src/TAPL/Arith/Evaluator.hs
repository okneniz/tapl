module TAPL.Arith.Evaluator where

import TAPL.Arith.Types
import TAPL.Arith.Parser
import Text.Parsec.Error
import Debug.Trace (traceM, trace)

eval :: String -> String -> Either ParseError String
eval code path = do
   ast <- parse code path
   return $ show $ last $ fullNormalize <$> ast

fullNormalize :: Term -> Term
fullNormalize c = case normalize c of
                       Just c' -> fullNormalize c'
                       Nothing -> c

normalize :: Term -> Maybe Term
normalize TTrue = Nothing
normalize TFalse = Nothing
normalize TZero = Nothing
normalize (TIf TTrue t _) = return t
normalize (TIf TFalse _ t) = return t

normalize (TIf t1 t2 t3) = do
  t1' <- normalize t1
  return $ TIf t1' t2 t3

normalize (TSucc t) = do
  t' <- normalize t
  return $ TSucc t'

normalize (TPred TZero) = return TZero
normalize (TPred (TSucc t)) | isNumerical t = return t

normalize (TPred t) = do
  t' <- normalize t
  return $ TPred t'

normalize (TIsZero TZero) = return TTrue
normalize (TIsZero (TSucc t)) | isNumerical t = return TFalse

normalize (TIsZero t) = do
  t' <- normalize t
  return $ TIsZero t
