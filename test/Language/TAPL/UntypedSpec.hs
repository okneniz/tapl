module Language.TAPL.UntypedSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.Untyped.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
      describe "abstractions" $ do
        tests evalString [
                ("(lambda x.x x)", pass "(lambda x.x x)"),
                ("(lambda x.lambda y.x y)", pass "(lambda x.(lambda y.x y))")
               ]

    describe "apply" $ do
        tests evalString [
            ("(lambda x.lambda y.lambda z.y z) (lambda z.z z)", pass "(lambda y.(lambda z.y z))"),
            ("(lambda x.lambda y.lambda z.x)(lambda z.z z)", pass "(lambda y.(lambda z.(lambda z'.z' z')))"),
            ("(lambda x.lambda y.lambda z.x) (lambda z.z z)(lambda z.z z)", pass "(lambda z.(lambda z'.z' z'))")
           ]
