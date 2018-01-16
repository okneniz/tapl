module TAPL.ArithSpec where

import Test.Hspec
import TAPL.Arith

spec :: Spec
spec = do
  describe "eval" $ do
    it "true -> true" $ do
        eval "true" `shouldBe` "true"

    it "(true) -> true" $ do
        eval "(true)" `shouldBe` "true"

    it "((true)) -> true" $ do
        eval "((true))" `shouldBe` "true"

    it "false -> false" $ do
        eval "false" `shouldBe` "false"

    it "(false) -> false" $ do
        eval "(false)" `shouldBe` "false"

    it "((false)) -> false" $ do
        eval "((false))" `shouldBe` "false"

    it "0 -> 0" $ do
        eval "0" `shouldBe` "0"

    it "(0) -> 0" $ do
        eval "(0)" `shouldBe` "0"

    it "((0)) -> 0" $ do
        eval "((0))" `shouldBe` "0"

    it "succ 0 -> succ 0" $ do
        eval "succ 0" `shouldBe` "succ 0"

    it "(succ 0) -> succ 0" $ do
        eval "(succ 0)" `shouldBe` "succ 0"

    it "((succ 0)) -> succ 0" $ do
        eval "((succ 0))" `shouldBe` "succ 0"

    it "isZero 0 -> true" $ do
        eval "isZero 0" `shouldBe` "true"

    it "(isZero 0) -> true" $ do
        eval "(isZero 0)" `shouldBe` "true"

    it "((isZero 0)) -> true" $ do
        eval "((isZero 0))" `shouldBe` "true"

    it "((isZero ((0)))) -> true" $ do
        eval "((isZero ((0))))" `shouldBe` "true"

    it "isZero (succ 0) -> false" $ do
        eval "isZero (succ 0)" `shouldBe` "false"

    it "pred (succ 0) -> 0" $ do
        eval "pred (succ 0)" `shouldBe` "0"

    it "pred succ 0 -> 0" $ do
        eval "pred succ 0" `shouldBe` "0"

    it "pred succ (0) -> 0" $ do
        eval "pred succ (0)" `shouldBe` "0"

    it "succ (pred (succ (0))) -> succ 0" $ do
        eval "succ (pred (succ (0)))" `shouldBe` "succ 0"

    it "succ (pred succ 0) -> succ 0" $ do
        eval "succ (pred succ 0)" `shouldBe` "succ 0"

    it "pred 0 -> 0" $ do
        eval "pred 0" `shouldBe` "0"

    it "succ pred pred pred pred 0 -> succ 0" $ do
        eval "succ pred pred pred pred 0" `shouldBe` "succ 0"

    it "succ (pred pred pred pred (0)) -> succ 0" $ do
        eval "succ (pred pred pred pred (0))" `shouldBe` "succ 0"

    it "if isZero 0 then succ 0 else succ pred 0 -> succ 0" $ do
        eval "if isZero 0 then succ 0 else succ pred 0" `shouldBe` "succ 0"

    it "if isZero succ 0 then succ 0 else pred 0 -> 0" $ do
        eval "if isZero succ 0 then succ 0 else pred 0" `shouldBe` "0"

    it "if isZero (succ 0) then succ 0 else 0 -> 0" $ do
        eval "if isZero (succ 0) then succ 0 else 0" `shouldBe` "0"

    it "if (succ 0) then succ 0 else 0 -> if succ 0 then succ 0 else 0" $ do
        eval "if (succ 0) then succ 0 else 0" `shouldBe` "if succ 0 then succ 0 else 0"

    it "if pred (succ 0) then succ 0 else 0 -> if 0 then succ 0 else 0" $ do
        eval "if pred (succ 0) then succ 0 else 0" `shouldBe` "if 0 then succ 0 else 0"

    it "if isZero (pred (succ 0)) then succ 0 else 0 -> succ 0" $ do
        eval "if isZero (pred (succ 0)) then succ 0 else 0" `shouldBe` "succ 0"

    it "if (isZero (pred (succ 0)) then succ 0 else 0 -> fail" $ do
        eval "if (isZero (pred (succ 0)) then succ 0 else 0" `shouldBe` "\"arith\" (line 1, column 28):\nunexpected \"t\"\nexpecting space, white space or \")\""


