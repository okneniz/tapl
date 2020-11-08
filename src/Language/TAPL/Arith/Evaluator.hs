module Language.TAPL.Arith.Evaluator (evalString) where

import Data.Text.Prettyprint.Doc (pretty)
import Language.TAPL.Common.Helpers (whileJust)
import Language.TAPL.Arith.Types
import Language.TAPL.Arith.Parser

evalString :: String -> Either String String
evalString code = do
   case parse code "<stdin>" of
        Left e -> Left $ show e
        Right ast -> return $ show
                            $ pretty
                            $ last
                            $ whileJust normalize <$> ast

normalize :: Term -> Maybe Term
normalize TTrue = Nothing
normalize TFalse = Nothing
normalize TZero = Nothing
normalize (TIf TTrue t _) = return t
normalize (TIf TFalse _ t) = return t
normalize (TIf t1 t2 t3) = TIf <$> normalize t1 <*> return t2 <*> return t3
normalize (TSucc t) = TSucc <$> normalize t
normalize (TPred TZero) = return TZero
normalize (TPred (TSucc t)) | isNumerical t = return t
normalize (TPred t) = TPred <$> normalize t
normalize (TIsZero TZero) = return TTrue
normalize (TIsZero (TSucc t)) | isNumerical t = return TFalse
normalize (TIsZero t) = TIsZero <$> normalize t
