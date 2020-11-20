module Language.TAPL.SimpleBoolSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.SimpleBool.Evaluator (evalString)


spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
            tests evalString [
                  ("true", pass "true:Bool"),
                  ("false", pass "false:Bool")
                ]

        describe "abstractions" $ do
            tests evalString [
                    ("(lambda x:Bool.x)", pass "(lambda x.x):(Bool -> Bool)"),
                    ("(lambda x:Bool.lambda y:Bool.x)", pass "(lambda x.(lambda y.x)):(Bool -> (Bool -> Bool))")
                 ]

    describe "operations" $ do
        describe "condition" $ do
            tests evalString [
                  ("if true then false else false", pass "false:Bool"),
                  ("if false then false else true", pass "true:Bool")
                ]

        describe "apply" $ do
            tests evalString [
                  ("(lambda x:Bool. if x then false else true) true", pass "false:Bool"),
                  (
                    "(lambda x:Bool.lambda y:(Bool -> (Bool -> Bool)). if x then y true else y false) true",
                    pass "(lambda y.if true then y true\n          else y false):((Bool -> (Bool -> Bool)) -> (Bool -> Bool))"
                  )
                 ]
