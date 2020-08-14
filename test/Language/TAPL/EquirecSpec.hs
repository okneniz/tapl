module Language.TAPL.EquirecSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.Equirec.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "abstractions" $ do
        tests evalString [
            (
                "(lambda x:A.x)",
                pass "(lambda x.x):A -> A"
            ),
            (
                "(lambda f:Rec X.A->A.lambda x:A. f x)",
                pass "(lambda f.(lambda x.f x)):Rec X.A -> A -> A -> A"
            )
           ]
