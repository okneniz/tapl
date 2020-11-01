module Language.TAPL.FOmegaSpec where
import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FOmega.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "abstractions" $ do
           tests evalString [
                    (
                        "lambda X. lambda x:X. x",
                        pass "(lambda X.(lambda x.x)):(All X::*. (X -> X))"
                    )
                 ]

        describe "apply" $ do
           tests evalString [
                    (
                        "(lambda X. lambda x:X. x) [All X.X->X]",
                        pass "(lambda x.x):((All X::*. (X -> X)) -> (All X::*. (X -> X)))"
                    )
                 ]
