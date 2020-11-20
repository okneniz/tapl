module Language.TAPL.PureFSubSpec where
import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.PureFSub.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "abstractions" $ do
           tests evalString [
                    (
                      "(lambda X.lambda x:X. x)",
                      pass "(lambda X.(lambda x.x)):(All X:Top<:(X -> X))"
                    ),
                    (
                      "(lambda x:Top. x)",
                      pass "(lambda x.x):(Top -> Top)"
                    )
                 ]

    describe "operations" $ do
        describe "apply" $ do
           tests evalString [
                    (
                      "(lambda X. lambda x:X. x) [All X.X->X]",
                      pass "(lambda x.x):((All X:Top<:(X -> X)) -> (All X:Top<:(X -> X)))"
                    ),
                    (
                      "(lambda x:Top. x) (lambda x:Top. x)",
                      pass "(lambda x.x):Top"
                    ),
                    (
                      "(lambda x:Top->Top. x) (lambda x:Top. x)",
                      pass "(lambda x.x):(Top -> Top)"
                    ),
                    (
                      "(lambda X<:Top->Top.lambda x:X.x x)",
                      pass "(lambda X.(lambda x.x x)):(All X:(Top -> Top)<:(X -> Top))"
                    )
                 ]

        describe "type binding" $ do
           tests evalString [
                    (
                      "T = Top->Top",
                      pass ""
                    ),
                    (
                      "B = Top->Top; (lambda x:B.x)",
                      pass "(lambda x.x):(B -> B)"
                    )
                 ]
