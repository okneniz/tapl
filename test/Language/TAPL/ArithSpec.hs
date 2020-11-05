module Language.TAPL.ArithSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.Arith.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "primitive values" $ do
       tests evalString [
          ("true", pass "true"),
          ("false", pass "false"),
          ("zero", pass "zero"),
          ("0", pass "zero"),
          ("5", pass "succ succ succ succ succ zero"),
          ("pred 5", pass "succ succ succ succ zero"),
          ("4", pass "succ succ succ succ zero")
        ]

    describe "condition" $ do
       tests evalString [
          ("if true then true else false", pass "true"),
          ("if true then false else false", pass "false"),
          ("if false then false else true", pass "true")
        ]

    describe "predefined functions" $ do
       tests evalString [
          ("succ zero", pass "succ zero"),
          ("pred zero", pass "zero"),
          ("pred succ zero", pass "zero"),
          ("succ pred pred zero", pass "succ zero"),
          ("zero? zero", pass "true"),
          ("zero? succ zero", pass "false"),
          ("zero? pred succ zero", pass "true")
         ]

    describe "invalid terms" $ do
       tests evalString [
          ("zero? zero? zero", pass "zero? true"),
          ("if succ zero then pred zero else false", pass "if succ zero then pred zero else false"),
          ("zero? false", pass "zero? false")
        ]
