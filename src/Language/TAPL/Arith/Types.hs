module Language.TAPL.Arith.Types where

import Data.Text.Prettyprint.Doc

data Term = TTrue
          | TFalse
          | TZero
          | TIf Term Term Term
          | TSucc Term
          | TPred Term
          | TIsZero Term

instance Pretty Term where
    pretty TTrue = pretty "true"
    pretty TFalse = pretty "false"
    pretty TZero = pretty "zero"
    pretty (TSucc x) = pretty "succ" <+> pretty x
    pretty (TPred x) = pretty "pred" <+> pretty x
    pretty (TIsZero x) = pretty "zero?" <+> pretty x
    pretty (TIf x y z) = align $ fillSep [ (pretty "if" <+> pretty x)
                                         , (pretty "then" <+> pretty y)
                                         , (pretty "else" <+> pretty z)
                                         ]
