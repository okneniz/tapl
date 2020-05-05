{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.RcdSubBotSpec where

import Test.Hspec
import Language.TAPL.RcdSubBot.Evaluator (evalString)

eval = evalString

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "records" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                    ("{}", "{}:{}"),
                    ("{x={y={},z={}}}", "{x={y={},z={}}}:{x:{y:{},z:{}}}"),
                    ("{x=(lambda y:Bot.y)}", "{x=(lambda y.y)}:{x:Bot -> Bot}")
                 ]
            mapM_ test examples

        describe "abstractions" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                    ("(lambda x:Top.x)", "(lambda x.x):Top -> Top"),
                    ("(lambda x:Bot.x)", "(lambda x.x):Bot -> Bot"),
                    ("(lambda r:{x:(Top -> Top)}.r)", "(lambda r.r):{x:Top -> Top} -> {x:Top -> Top}"),
                    ("(lambda r:{x:(Top -> Top)}. r.x)", "(lambda r.r.x):{x:Top -> Top} -> Top -> Top")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bot.x x)", "(lambda x.x x):Bot -> Bot"),
                  ("(lambda x:Top.x)(lambda x:Top.x)", "(lambda x.x):Top -> Top"),
                  ("(lambda r:{x:(Top -> Top)}. r.x r.x)", "(lambda r.r.x r.x):{x:Top -> Top} -> Top"),
                  ("(lambda r:{x:{}}.r)", "(lambda r.r):{x:{}} -> {x:{}}"),
                  ("{x=(lambda z:Top.z), y=(lambda z:Top.z)}", "{x=(lambda z.z),y=(lambda z.z)}:{x:Top -> Top,y:Top -> Top}"),
                  ("(lambda x:(Top -> Top). x) (lambda x:Top. x)", "(lambda x.x):Top -> Top")
                 ]
            mapM_ test examples
