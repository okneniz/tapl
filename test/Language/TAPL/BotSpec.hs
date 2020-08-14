module Language.TAPL.BotSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.Bot.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "abstractions" $ do
            tests evalString [
                ("(lambda x:Top.x)", pass "(lambda x.x):Top -> Top"),
                ("(lambda x:Bot.x)", pass "(lambda x.x):Bot -> Bot")
             ]

    describe "operations" $ do
        describe "apply" $ do
            tests evalString [
              ("(lambda x:Top. x) (lambda x:Top. x)", pass "(lambda x.x):Top -> Top"),
              ("(lambda x:Bot.x x)", pass "(lambda x.x x):Bot -> Bot"),
              ("(lambda x:Top -> Top. x) (lambda x:Top. x)", pass "(lambda x.x):Top -> Top")
             ]
