module Language.TAPL.TypedArithSpec where

import Test.Hspec
import Language.TAPL.TypedArith.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("true", "true:Bool"),
                  ("false", "false:Bool"),
                  ("zero", "zero:Nat")
                ]
           mapM_ test examples

        describe "abstractions" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                    ("(lambda x:Bool.x)", "(lambda x.x):(Bool -> Bool)"),
                    ("(lambda x:Bool.lambda y:Bool.x)", "(lambda x.(lambda y.x)):(Bool -> (Bool -> Bool))"),
                    ("(lambda x:Bool.lambda y:Nat.x)", "(lambda x.(lambda y.x)):(Bool -> (Nat -> Bool))")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "condition" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("if true then false else false", "false:Bool"),
                  ("if false then false else true", "true:Bool"),
                  ("if zero? (succ zero) then false else true", "true:Bool")
                ]
           mapM_ test examples

        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bool. if x then false else true) true", "false:Bool"),
                  ("(lambda x:Nat. if zero? x then false else true) succ zero", "true:Bool"),
                  ("(lambda x:Bool.lambda y:(Bool -> (Bool -> Bool)). if x then y true else y false) true",
                    "(lambda y.if true then y true else y false):((Bool -> (Bool -> Bool)) -> (Bool -> Bool))"
                  )
                 ]
            mapM_ test examples
