module Language.TAPL.FullUntypedSpec where
import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FullUntyped.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
           tests evalString [
              ("true", pass "true"),
              ("false", pass "false"),
              ("\"foo\"", pass "\"foo\""),
              ("unit", pass "unit"),
              ("1.1", pass "1.1"),
              ("1", pass "1"),
              ("1.1000001", pass "1.1000001")
            ]

        describe "pairs" $ do
           tests evalString [
              ("{true, false}", pass "{true,false}"),
              ("{true, unit}", pass "{true,unit}"),
              ("{1.1, \"foo\"}", pass "{1.1,\"foo\"}"),
              ("{(lambda x.x), (lambda x.x)}", pass "{(lambda x.x),(lambda x.x)}"),
              ("{true, false}.0", pass "true"),
              ("{true, unit}.1", pass "unit"),
              ("{1.1, \"foo\"}.0", pass "1.1"),
              ("{1.1, \"foo\"}.1", pass "\"foo\""),
              ("{(lambda x.x), (lambda x.x)}.1", pass "(lambda x.x)")
            ]

        describe "records" $ do
           tests evalString [
                  ("{a=true, b=false}", pass "{a=true, b=false}"),
                  ("{a=true, b=unit}", pass "{a=true, b=unit}"),
                  ("{a=1.1, b=\"foo\"}", pass "{a=1.1, b=\"foo\"}"),
                  ("{c=(lambda x.x), d=(lambda x.x)}", pass "{c=(lambda x.x), d=(lambda x.x)}"),
                  ("{a=true, b=false}.a", pass "true"),
                  ("{a=true, b=unit}.b", pass "unit"),
                  ("{a=1.1, b=\"foo\"}.b", pass "\"foo\""),
                  ("{c=(lambda x.x), d=(lambda x.x)}.d", pass "(lambda x.x)"),
                  ("{c=(lambda x.x), d=((lambda x.x) unit)}.d", pass "unit"),
                  ("{c=(lambda x.x), d=(lambda x.x)}.d unit", pass "unit")
                ]

        describe "abstractions" $ do
           tests evalString [
                    ("(lambda x.x)", pass "(lambda x.x)"),
                    ("(lambda x.lambda y.x)", pass "(lambda x.(lambda y.x))"),
                    ("(lambda x.(lambda y.x))", pass "(lambda x.(lambda y.x))")
                 ]

    describe "operations" $ do
        describe "condition" $ do
           tests evalString [
                  ("if true then unit else unit", pass "unit"),
                  ("if false then \"foo\" else \"bar\"", pass "\"bar\""),
                  ("if true then 3.14 else 9.8", pass "3.14")
                ]

        describe "predefined functions" $ do
           tests evalString [
                  ("succ zero", pass "succ zero"),
                  ("pred zero", pass "zero"),
                  ("pred succ zero", pass "zero"),
                  ("succ pred pred zero", pass "succ zero"),
                  ("zero? zero", pass "true"),
                  ("zero? succ zero", pass "false"),
                  ("timesfloat 1.0 2.0", pass "2.0"),
                  ("timesfloat (timesfloat 1.3 1.111) 2.0", pass "2.8886000000000003"),
                  ("timesfloat (timesfloat 3.14 2.0) 10.0", pass "62.800000000000004")
                 ]

        describe "apply" $ do
           tests evalString [
                  ("(lambda x.if x then false else true) true", pass "false"),
                  ("(lambda x.succ x) zero", pass "succ zero"),
                  ("(lambda x.lambda y. x true)", pass "(lambda x.(lambda y.x true))"),
                  ("(lambda x.succ x) succ zero", pass "succ succ zero"),
                  ("(lambda x.lambda y.lambda z.if zero? x then y else z) pred zero", pass "(lambda y.(lambda z.if zero? zero then y else z))"),
                  ("(lambda x.lambda y.lambda z.if zero? x then y else z) zero 3.14 9.8", pass "3.14"),
                  ("(lambda x.x zero) (lambda x.succ x)", pass "succ zero"),
                  ("(lambda x. \
                    \lambda y. \
                    \lambda z. \
                    \lambda p. \
                    \if x y then z else p) (lambda x. zero? x) (succ zero) 3.14",
                    pass "(lambda p.if (lambda x.zero? x) succ zero then 3.14 else p)"
                  ),
                  ("(lambda x. \
                    \lambda y. \
                    \lambda z. \
                    \lambda p. \
                    \if x y then z else p) (lambda x.zero? x) (succ zero) 3.14 9.8", pass "9.8"),
                  ("(lambda x.x) {a=true, b=false}", pass "{a=true, b=false}"),
                  ("(lambda x.if x.a then false else true) {a=true, b=false}", pass "false")
                 ]
