module Language.TAPL.FullReconSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FullRecon.Evaluator (evalString)

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
                    (
                      "(lambda x:Bool->Bool. if x false then true else false)",
                      pass "(lambda x.if x false then true else false):((Bool -> Bool) -> Bool)"
                    ),
                    (
                      "(lambda x:Nat. succ x)",
                      pass "(lambda x.succ x):(Nat -> Nat)"
                    ),
                    (
                      "(lambda x:Nat. succ (succ x)) (succ zero)",
                      pass "succ succ succ zero:Nat"
                    ),
                    ("(lambda x:Bool.x)", pass "(lambda x.x):(Bool -> Bool)"),
                    ("(lambda x:Bool.lambda y:A.x)", pass "(lambda x.(lambda y.x)):(Bool -> (A -> Bool))"),
                    ("(lambda x:Bool.lambda y:A.y)", pass "(lambda x.(lambda y.y)):(Bool -> (A -> A))"),
                    ("(lambda x:(Bool -> A).lambda y:A.y)", pass "(lambda x.(lambda y.y)):((Bool -> A) -> (A -> A))"),
                    ("(lambda x:A -> A.lambda y:A.x y)", pass "(lambda x.(lambda y.x y)):((A -> A) -> (A -> x0))"),
                    ("(lambda x:A.x)", pass "(lambda x.x):(A -> A)"),
                    ("(lambda x.x zero);", pass "(lambda x.x zero):(x1 -> x2)"),
                    ("(lambda x:X.lambda y:X->X.y x)", pass "(lambda x.(lambda y.y x)):(X -> ((X -> X) -> x0))")
                 ]

    describe "operations" $ do
        describe "condition" $ do
            tests evalString [
                  ("if true then succ zero else zero", pass "succ zero:Nat"),
                  ("if false then zero else succ zero", pass "succ zero:Nat")
                ]

        describe "predefined functions" $ do
            tests evalString [
                  ("succ zero", pass "succ zero:Nat"),
                  ("pred zero", pass "zero:Nat"),
                  ("pred succ zero", pass "zero:Nat"),
                  ("succ pred pred zero", pass "succ zero:Nat"),
                  ("zero? zero", pass "true:Bool"),
                  ("zero? succ zero", pass "false:Bool")
                 ]

        describe "apply" $ do
            tests evalString [
                  ("(lambda x:Bool. if x then false else true) true", pass "false:Bool"),
                  ("(lambda x:Nat. succ x) zero", pass "succ zero:Nat"),
                  (
                    "(lambda x:Bool -> Bool.lambda y:A. x true)",
                    pass "(lambda x.(lambda y.x true)):((Bool -> Bool) -> (A -> x0))"
                  ),
                  ("(lambda x:Nat. succ x) succ zero", pass "succ succ zero:Nat"),
                  ("(lambda x:Nat -> Nat. x zero) (lambda x:Nat. succ x)", pass "succ zero:Nat"),
                  (
                    "(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then false else true);",
                    pass "true:Bool"
                  ),
                  ("(lambda x. x zero);", pass "(lambda x.x zero):(x1 -> x2)"),
                  ("(lambda x:X->X. x zero) (lambda y:Nat. y)", pass "zero:Nat")
                 ]

        describe "let" $ do
            tests evalString [
                  ("let x = true in x", pass "true:Bool"),
                  ("let x = (lambda x:Nat.x) in x", pass "(lambda x'.x'):(Nat -> Nat)"),
                  ("let f = (lambda x.x) in (f f) (f zero)", pass "zero:Nat"),
                  ("let g = (lambda x. succ zero) in g (g g)", pass "succ zero:Nat")
                 ]
