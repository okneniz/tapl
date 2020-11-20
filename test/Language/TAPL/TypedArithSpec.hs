module Language.TAPL.TypedArithSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.TypedArith.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
            tests evalString [
                  ("true", pass "true:Bool"),
                  ("false", pass "false:Bool"),
                  ("zero", pass "zero:Nat"),
                  ("0", pass "zero:Nat"),
                  ("5", pass "succ succ succ succ succ zero:Nat"),
                  ("pred 5", pass "succ succ succ succ zero:Nat"),
                  ("4", pass "succ succ succ succ zero:Nat")
                ]

        describe "abstractions" $ do
            tests evalString [
                    ("(lambda x:Bool.x)", pass "(lambda x.x):(Bool -> Bool)"),
                    ("(lambda x:Bool.lambda y:Bool.x)", pass "(lambda x.(lambda y.x)):(Bool -> (Bool -> Bool))"),
                    ("(lambda x:Bool.lambda y:Nat.x)", pass "(lambda x.(lambda y.x)):(Bool -> (Nat -> Bool))")
                 ]

    describe "operations" $ do
        describe "condition" $ do
            tests evalString [
                  ("if true then false else false", pass "false:Bool"),
                  ("if false then false else true", pass "true:Bool"),
                  ("if zero? (succ zero) then false else true", pass "true:Bool")
                ]

        describe "apply" $ do
            tests evalString [
                  ("(lambda x:Bool. if x then false else true) true", pass "false:Bool"),
                  ("(lambda x:Nat. if zero? x then false else true) succ zero", pass "true:Bool"),
                  (
                    "(lambda x:Bool.lambda y:(Bool -> (Bool -> Bool)). if x then y true else y false) true",
                    pass "(lambda y.if true then y true\n          else y false):((Bool -> (Bool -> Bool)) -> (Bool -> Bool))"
                  ),
                  (
                    "(lambda x:Nat.if zero? x then succ x else x) 0",
                    pass "succ zero:Nat"
                  ),
                  (
                    "(lambda x:Nat.if zero? x then succ x else x) 3",
                    pass "succ succ succ zero:Nat"
                  )
                 ]
