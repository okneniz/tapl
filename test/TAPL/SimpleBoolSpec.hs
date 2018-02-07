module TAPL.SimpleBoolSpec where

import Test.Hspec
import TAPL.SimpleBool

spec :: Spec
spec = do
  describe "eval" $ do
    it "true" $ do
        eval "true" `shouldBe` "true:Bool"

    it "false" $ do
        eval "false" `shouldBe` "false:Bool"

    it "if true then true else false -> true:Bool" $ do
        eval "if true then true else false" `shouldBe` "true:Bool"

    it "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true -> (λy.(λz.if true then y else z)):(Bool->Bool)" $ do
        eval "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true" `shouldBe` "(λy.(λz.if true then y else z)):(Bool->Bool)"

    it "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true -> (λz.if true then true else z):(Bool->Bool)" $ do
        eval "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true" `shouldBe` "(λz.if true then true else z):(Bool->Bool)"

    it "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true false -> true:Bool" $ do
        eval "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true false" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true) -> true:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true)" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true) -> true:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true)" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true) -> true:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true)" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then true else false) -> false:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then true else false)" `shouldBe` "false:Bool"
