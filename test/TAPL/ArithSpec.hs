module TAPL.ArithSpec where

import Test.Hspec
import TAPL.Arith.Evaluator (eval)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "primitive values" $ do
       let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
           examples = [
              ("true", "true"),
              ("false", "false"),
              ("zero", "zero")
            ]
       mapM_ test examples

    describe "condition" $ do
       let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
           examples = [
              ("if true then true else false", "true"),
              ("if true then false else false", "false"),
              ("if false then false else true", "true")
            ]
       mapM_ test examples

    describe "predefined functions" $ do
        let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
            examples = [
              ("succ zero", "succ zero"),
              ("pred zero", "zero"),
              ("pred succ zero", "zero"),
              ("succ pred pred zero", "succ zero"),
              ("zero? zero", "true"),
              ("zero? succ zero", "false")
             ]
        mapM_ test examples
