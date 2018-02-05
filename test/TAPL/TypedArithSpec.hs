module TAPL.TypedArithSpec where

import Test.Hspec
import TAPL.TypedArith

spec :: Spec
spec = do
  describe "eval" $ do
    it "true -> true:Bool" $ do
        eval "true" `shouldBe` "true:Bool"

    it "(true) -> true:Bool" $ do
        eval "(true)" `shouldBe` "true:Bool"

    it "((true)) -> true:Bool" $ do
        eval "((true))" `shouldBe` "true:Bool"

    it "false -> false:Bool" $ do
        eval "false" `shouldBe` "false:Bool"

    it "(false) -> false:Bool" $ do
        eval "(false)" `shouldBe` "false:Bool"

    it "((false)) -> false:Bool" $ do
        eval "((false))" `shouldBe` "false:Bool"

    it "0 -> 0:Nat" $ do
        eval "0" `shouldBe` "0:Nat"

    it "(0) -> 0:Nat" $ do
        eval "(0)" `shouldBe` "0:Nat"

    it "((0)) -> 0:Nat" $ do
        eval "((0))" `shouldBe` "0:Nat"

    it "succ 0 -> succ 0:Nat" $ do
        eval "succ 0" `shouldBe` "succ 0:Nat"

    it "(succ 0) -> succ 0:Nat" $ do
        eval "(succ 0)" `shouldBe` "succ 0:Nat"

    it "((succ 0)) -> succ 0:Nat" $ do
        eval "((succ 0))" `shouldBe` "succ 0:Nat"

    it "isZero 0 -> true:Bool" $ do
        eval "isZero 0" `shouldBe` "true:Bool"

    it "(isZero 0) -> true:Bool" $ do
        eval "(isZero 0)" `shouldBe` "true:Bool"

    it "((isZero 0)) -> true:Bool" $ do
        eval "((isZero 0))" `shouldBe` "true:Bool"

    it "((isZero ((0)))) -> true:Bool" $ do
        eval "((isZero ((0))))" `shouldBe` "true:Bool"

    it "isZero (succ 0) -> false:Bool" $ do
        eval "isZero (succ 0)" `shouldBe` "false:Bool"

    it "pred (succ 0) -> 0:Nat" $ do
        eval "pred (succ 0)" `shouldBe` "0:Nat"

    it "pred succ 0 -> 0:Nat" $ do
        eval "pred succ 0" `shouldBe` "0:Nat"

    it "pred succ (0) -> 0:Nat" $ do
        eval "pred succ (0)" `shouldBe` "0:Nat"

    it "succ (pred (succ (0))) -> succ 0:Nat" $ do
        eval "succ (pred (succ (0)))" `shouldBe` "succ 0:Nat"

    it "succ (pred succ 0) -> succ 0:Nat" $ do
        eval "succ (pred succ 0)" `shouldBe` "succ 0:Nat"

    it "pred 0 -> 0:Nat" $ do
        eval "pred 0" `shouldBe` "0:Nat"

    it "succ pred pred pred pred 0 -> succ 0:Nat" $ do
        eval "succ pred pred pred pred 0" `shouldBe` "succ 0:Nat"

    it "succ (pred pred pred pred (0)) -> succ 0:Nat" $ do
        eval "succ (pred pred pred pred (0))" `shouldBe` "succ 0:Nat"

    it "if isZero 0 then succ 0 else succ pred 0 -> succ 0:Nat" $ do
        eval "if isZero 0 then succ 0 else succ pred 0" `shouldBe` "succ 0:Nat"

    it "if isZero succ 0 then succ 0 else pred 0 -> 0:Nat" $ do
        eval "if isZero succ 0 then succ 0 else pred 0" `shouldBe` "0:Nat"

    it "if isZero (succ 0) then succ 0 else 0 -> 0:Nat" $ do
        eval "if isZero (succ 0) then succ 0 else 0" `shouldBe` "0:Nat"

    it "if (succ 0) then succ 0 else 0 -> guard of conditional not a boolean" $ do
        eval "if (succ 0) then succ 0 else 0" `shouldBe` "guard of conditional not a boolean"

    it "if pred (succ 0) then succ 0 else 0 -> guard of conditional not a boolean" $ do
        eval "if pred (succ 0) then succ 0 else 0" `shouldBe` "guard of conditional not a boolean"

    it "if isZero (pred (succ 0)) then succ 0 else 0 -> succ 0:Nat" $ do
        eval "if isZero (pred (succ 0)) then succ 0 else 0" `shouldBe` "succ 0:Nat"

    it "if true then succ 0 else 0 -> succ 0:Nat" $ do
        eval "if true then succ 0 else 0" `shouldBe` "succ 0:Nat"

    it "if false then succ 0 else 0 -> succ 0:Nat" $ do
        eval "if false then succ 0 else 0" `shouldBe` "0:Nat"

    it "if false then succ 0 else true -> succ 0:Nat" $ do
        eval "if false then succ 0 else true" `shouldBe` "arms of conditional have different types"
        
    it "if false then true else pred 0 -> succ 0:Nat" $ do
        eval "if false then true else pred 0" `shouldBe` "arms of conditional have different types"

    it "if (isZero (pred (succ 0)) then succ 0 else 0 -> fail" $ do
        eval "if (isZero (pred (succ 0)) then succ 0 else 0" `shouldBe` "\"arith\" (line 1, column 28):\nunexpected \"t\"\nexpecting space, white space or \")\""
