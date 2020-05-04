{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullErrorSpec where

import Test.Hspec
import Language.TAPL.FullError.Evaluator (evalString)

eval = evalString

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("true", "true:Bool"),
                  ("false", "false:Bool"),
                  ("error", "error:Bot")
                ]
           mapM_ test examples

        describe "abstractions" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                    ("(lambda x:Bool.x)", "(lambda x.x):Bool -> Bool"),
                    ("(lambda x:Bot.error)", "(lambda x.error):Bot -> Bot")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "condition" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("if true then false else true", "false:Bool"),
                  ("if true then false else error", "false:Bool"),
                  ("if true then error else true", "error:Bot"),
                  ("if false then false else true", "true:Bool"),
                  ("if false then error else true", "true:Bool"),
                  ("if false then false else error", "error:Bot")
                ]
           mapM_ test examples

        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bool. if x then false else true) true", "false:Bool"),
                  ("(lambda x:Bool.error) true", "error:Bot"),
                  ("(lambda x:Bool.if x then error else false) true", "error:Bot"),
                  ("(lambda x:Bool.if x then error else false) false", "false:Bool")
                 ]
            mapM_ test examples
