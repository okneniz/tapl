{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullReconSpec where

import Test.Hspec
import TAPL.FullRecon.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("true", "true:Bool"),
                  ("false", "false:Bool")
                ]
           mapM_ test examples

        describe "abstractions" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                    (
                      "(lambda x:Bool->Bool. if x false then true else false)",
                      "(lambda x.if x false then true else false):((Bool -> Bool) -> Bool)"
                    ),
                    (
                      "(lambda x:Nat. succ x)",
                      "(lambda x.succ x):(Nat -> Nat)"
                    ),
                    (
                      "(lambda x:Nat. succ (succ x)) (succ zero)",
                      "succ succ succ zero:Nat"
                    ),
                    ("(lambda x:Bool.x)", "(lambda x.x):(Bool -> Bool)"),
                    ("(lambda x:Bool.lambda y:A.x)", "(lambda x.(lambda y.x)):(Bool -> (A -> Bool))"),
                    ("(lambda x:Bool.lambda y:A.y)", "(lambda x.(lambda y.y)):(Bool -> (A -> A))"),
                    ("(lambda x:(Bool -> A).lambda y:A.y)", "(lambda x.(lambda y.y)):((Bool -> A) -> (A -> A))"),
                    ("(lambda x:A -> A.lambda y:A.x y)", "(lambda x.(lambda y.x y)):((x0 -> x0) -> (x0 -> x0))"),
                    ("(lambda x:A.x)", "(lambda x.x):(A -> A)"),
                    ("(lambda x. x zero);", "(lambda x.x zero):((Nat -> x3) -> x3)"),
                    ("(lambda x:X. lambda y:X->X. y x)", "(lambda x.(lambda y.y x)):(x0 -> ((x0 -> x0) -> x0))")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "condition" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("if true then succ zero else zero", "succ zero:Nat"),
                  ("if false then zero else succ zero", "succ zero:Nat")
                ]
           mapM_ test examples

        describe "predefined functions" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("succ zero", "succ zero:Nat"),
                  ("pred zero", "zero:Nat"),
                  ("pred succ zero", "zero:Nat"),
                  ("succ pred pred zero", "succ zero:Nat"),
                  ("zero? zero", "true:Bool"),
                  ("zero? succ zero", "false:Bool")
                 ]
            mapM_ test examples

        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bool. if x then false else true) true", "false:Bool"),
                  ("(lambda x:Nat. succ x) zero", "succ zero:Nat"),
                  ("(lambda x:Bool -> Bool.lambda y:A. x true)", "(lambda x.(lambda y.x true)):((Bool -> Bool) -> (A -> Bool))"),
                  ("(lambda x:Nat. succ x) succ zero", "succ succ zero:Nat"),
                  ("(lambda x:Nat -> Nat. x zero) (lambda x:Nat. succ x)", "succ zero:Nat"),
                  (
                    "(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then false else true);",
                    "true:Bool"
                  ),
                  ("(lambda x. x zero);", "(lambda x.x zero):((Nat -> x3) -> x3)"),
                  ("(lambda x:X->X. x zero) (lambda y:Nat. y)","zero:Nat")
                 ]
            mapM_ test examples

        describe "let" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("let x = true in x", "true:Bool"),
                  ("let x = (lambda x:Nat.x) in x", "(lambda x'.x'):(Nat -> Nat)"),
                  ("let f = (lambda x.x) in (f f) (f zero)", "zero:Nat"),
                  ("let g = (lambda x. succ zero) in g (g g)", "succ zero:Nat")
                 ]
            mapM_ test examples

