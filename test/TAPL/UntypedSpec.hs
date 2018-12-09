module TAPL.UntypedSpec where

import Test.Hspec
import TAPL.Untyped.Evaluator (eval)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
      describe "abstractions" $ do
          let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
              examples = [
                ("(lambda x.x x)", "(lambda x.x x)"),
                ("(lambda x.lambda y.x y)", "(lambda x.(lambda y.x y))")
               ]
          mapM_ test examples

    describe "apply" $ do
      let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
          examples = [
            ("(lambda x.lambda y.lambda z.y z) (lambda z.z z)", "(lambda y.(lambda z.y z))"),
            ("(lambda x.lambda y.lambda z.x)(lambda z.z z)", "(lambda y.(lambda z.(lambda z'.z' z')))")
           ]
      mapM_ test examples
