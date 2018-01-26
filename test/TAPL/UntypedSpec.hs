module TAPL.UntypedSpec where

import Test.Hspec
import TAPL.Untyped

spec :: Spec
spec = do
  describe "eval" $ do
    it "(λx.xx) -> (λx.xx)" $ do
        eval "(λx.xx)" `shouldBe` "(λx.xx)"

    it "(λx.xx)(λx.xx) -> (λx.xx)(λx.xx)" $ do
        eval "(λx.xx)(λx.xx)" `shouldBe` "(λx.xx)(λx.xx)"

    it "(λx.λy.λz.x)(λz.zz) -> (λy.λz.λz.z'z')" $ do
        eval "(λx.λy.λz.x)(λz.zz)" `shouldBe` "(λy.(λz.(λz'.z'z')))"

    it "x -> x" $ do -- current implementation does not support free variables
        eval "x" `shouldBe` "\"untyped \955-calculus\" (line 1, column 2):\nunexpected end of input\nexpecting \"'\"\nvariable x has't been bound in context"
