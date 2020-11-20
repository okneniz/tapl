module Language.TAPL.FullErrorSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FullError.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
            tests evalString [
                  ("true", pass "true:Bool"),
                  ("false", pass "false:Bool"),
                  ("error", pass "error:Bot")
                ]

        describe "abstractions" $ do
            tests evalString [
                    ("(lambda x:Bool.x)", pass "(lambda x.x):Bool -> Bool"),
                    ("(lambda x:Bot.error)", pass "(lambda x.error):Bot -> Bot"),
                    (
                      "(lambda x:Bool.if x then error else false)",
                      pass "(lambda x.if x then error else false):Bool -> Top"
                    )
                 ]

    describe "operations" $ do
        describe "condition" $ do
            tests evalString [
                  ("if true then false else true", pass "false:Bool"),
                  ("if true then false else error", pass "false:Top"),
                  ("if true then error else true", pass "error:Top"),
                  ("if false then false else true", pass "true:Bool"),
                  ("if false then error else true", pass "true:Top"),
                  ("if false then false else error", pass "error:Top")
                ]

        describe "apply" $ do
            tests evalString [
                  ("(lambda x:Bool. if x then false else true) true", pass "false:Bool"),
                  ("(lambda x:Bool.error) true", pass "error:Bot"),
                  ("(lambda x:Bool.if x then error else false) true", pass "error:Top"),
                  ("(lambda x:Bool.if x then error else false) false", pass "false:Top")
                 ]
