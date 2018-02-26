{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullRefSpec where

import Test.Hspec
import TAPL.FullRef.Evaluator (eval)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("true", "true:Bool"),
                  ("false", "false:Bool"),
                  ("\"foo\"", "\"foo\":String"),
                  ("unit", "unit:Unit"),
                  ("1.1", "1.1:Float"),
                  ("1.1000001", "1.1000001:Float")
                ]
           mapM_ test examples

        describe "references" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("ref true", "<0>:Ref Bool"),
                  ("ref false", "<0>:Ref Bool"),
                  ("ref \"foo\"", "<0>:Ref String"),
                  ("ref unit", "<0>:Ref Unit"),
                  ("ref 1.1", "<0>:Ref Float"),
                  ("ref 1.1000001", "<0>:Ref Float"),
                  ("ref unit; ref true; ref false; ref 1.1000001", "<3>:Ref Float"),
                  ("ref ref ref ref unit", "<3>:Ref Ref Ref Ref Unit")
                ]
           mapM_ test examples

        describe "abstractions" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                    ("(lambda x:Bool.x)", "(lambda x.x):Bool -> Bool"),
                    ("(lambda x:String.x)", "(lambda x.x):String -> String"),
                    ("(lambda x:Unit.x)", "(lambda x.x):Unit -> Unit"),
                    ("(lambda x:Float.x)", "(lambda x.x):Float -> Float")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "condition" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("if true then unit else unit", "unit:Unit"),
                  ("if false then \"foo\" else \"bar\"", "\"bar\":String")
                ]
           mapM_ test examples

        describe "predefined functions" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("succ zero", "succ zero:Nat"),
                  ("pred zero", "zero:Nat"),
                  ("pred succ zero", "zero:Nat"),
                  ("succ pred pred zero", "succ zero:Nat"),
                  ("zero? zero", "true:Bool"),
                  ("zero? succ zero", "false:Bool")
                 ]
            mapM_ test examples

        describe "deref references" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("!(ref zero)", "zero:Nat"),
                  ("!(ref true)", "true:Bool"),
                  ("!(ref 12.1)", "12.1:Float"),
                  ("!(ref \"123\")", "\"123\":String")
                 ]
            mapM_ test examples

        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bool. if x then false else true) true", "false:Bool"),
                  ("(lambda x:Nat. succ x) zero", "succ zero:Nat"),
                  ("(lambda x:Nat. succ x) succ zero", "succ succ zero:Nat"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) pred zero", "(lambda y.(lambda z.if zero? zero then y else z)):Float -> Float -> Float"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) zero 3.14 9.8", "3.14:Float"),
                  ("if true then 3.14 else 9.8", "3.14:Float"),
                  ("(lambda x:Nat -> Nat. x zero) (lambda x:Nat. succ x)", "succ zero:Nat"),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14", "(lambda p.if (lambda x.zero? x) succ zero then 3.14 else p):Float -> Float"),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14 9.8", "9.8:Float")
                 ]
            mapM_ test examples

        describe "assignment and let" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(ref true) := false", "unit:Unit"),
                  ("(lambda x:Ref Bool. x := (if (!x) then false else true)) (ref false)", "unit:Unit"),
                  ("let x = ref zero in !x", "zero:Nat"),
                  ("let x = ref zero in let y = x := (succ !x) in x", "<0>:Ref Nat"),
                  ("let x = ref zero in let y = x := (succ !x) in !x", "succ zero:Nat"),
                  ("let x = ref zero in let y = x := (succ !x) in y", "unit:Unit")
                 ]
            mapM_ test examples
