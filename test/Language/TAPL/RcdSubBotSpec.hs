module Language.TAPL.RcdSubBotSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.RcdSubBot.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "records" $ do
            tests evalString [
                    ("{}", pass "{}:{}"),
                    ("{x={y={},z={}}}", pass "{x={y={},z={}}}:{x:{y:{},z:{}}}"),
                    ("{x=(lambda y:Bot.y)}", pass "{x=(lambda y.y)}:{x:Bot -> Bot}")
                 ]

        describe "abstractions" $ do
            tests evalString [
                    ("(lambda x:Top.x)", pass "(lambda x.x):Top -> Top"),
                    ("(lambda x:Bot.x)", pass "(lambda x.x):Bot -> Bot"),
                    ("(lambda r:{x:(Top -> Top)}.r)", pass "(lambda r.r):{x:Top -> Top} -> {x:Top -> Top}"),
                    ("(lambda r:{x:(Top -> Top)}. r.x)", pass "(lambda r.r.x):{x:Top -> Top} -> Top -> Top")
                 ]

    describe "operations" $ do
        describe "apply" $ do
            tests evalString [
                  ("(lambda x:Bot.x x)", pass "(lambda x.x x):Bot -> Bot"),
                  ("(lambda x:Top.x)(lambda x:Top.x)", pass "(lambda x.x):Top -> Top"),
                  ("(lambda r:{x:(Top -> Top)}. r.x r.x)", pass "(lambda r.r.x r.x):{x:Top -> Top} -> Top"),
                  ("(lambda r:{x:{}}.r)", pass "(lambda r.r):{x:{}} -> {x:{}}"),
                  ("{x=(lambda z:Top.z), y=(lambda z:Top.z)}", pass "{x=(lambda z.z),y=(lambda z.z)}:{x:Top -> Top,y:Top -> Top}"),
                  ("(lambda x:(Top -> Top). x) (lambda x:Top. x)", pass "(lambda x.x):Top -> Top")
                 ]
