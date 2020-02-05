module Language.TAPL.BotSpec where

import Test.Hspec
import Language.TAPL.Bot.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "abstractions" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                    ("(lambda x:Top.x)", "(lambda x.x):Top -> Top"),
                    ("(lambda x:Bot.x)", "(lambda x.x):Bot -> Bot")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bot.x x)", "(lambda x.x x):Bot -> Bot"),
                  ("(lambda x:Top -> Top. x) (lambda x:Top. x)", "(lambda x.x):Top -> Top")
                 ]
            mapM_ test examples
