module Language.TAPL.FullPolySpec where
import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FullPoly.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
           tests evalString [
              ("true", pass "true:Bool"),
              ("false", pass "false:Bool"),
              ("\"foo\"", pass "\"foo\":String"),
              ("unit", pass "unit:Unit"),
              ("1.1", pass "1.1:Float"),
              ("1.1000001", pass "1.1000001:Float"),
              ("zero", pass "zero:Nat"),
              ("0", pass "zero:Nat"),
              ("5", pass "succ succ succ succ succ zero:Nat"),
              ("pred 5", pass "succ succ succ succ zero:Nat"),
              ("4", pass "succ succ succ succ zero:Nat")
            ]

        describe "records" $ do
           tests evalString [
                  ("{a=true, b=false}", pass "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("{a=true, b=unit}", pass "{a=true, b=unit}:{a=Bool, b=Unit}"),
                  ("{a=1.1, b=\"foo\"}", pass "{a=1.1, b=\"foo\"}:{a=Float, b=String}"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}", pass "{c=(lambda x.x), d=(lambda x.x)}:{c=(Unit -> Unit), d=(Unit -> Unit)}"),
                  ("{a=true, b=false}.a", pass "true:Bool"),
                  ("{a=true, b=unit}.b", pass "unit:Unit"),
                  ("{a=1.1, b=\"foo\"}.b", pass "\"foo\":String"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d", pass "(lambda x.x):(Unit -> Unit)"),
                  ("{c=(lambda x:Unit.x), d=((lambda x:Unit.x) unit)}.d", pass "unit:Unit"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d unit", pass "unit:Unit")
                ]

        describe "ascribe" $ do
           tests evalString [
                  ("\"foo\" as String", pass "\"foo\":String"),
                  ("false as Bool", pass "false:Bool"),
                  ("1.1 as Float", pass "1.1:Float"),
                  ("true as Bool", pass "true:Bool"),
                  ("unit as Unit", pass "unit:Unit"),
                  ("1.1000001 as Float", pass "1.1000001:Float")
                ]

        describe "abstractions" $ do
           tests evalString [
                    ("(lambda x:Bool.x)", pass "(lambda x.x):(Bool -> Bool)"),
                    ("(lambda x:String.x)", pass "(lambda x.x):(String -> String)"),
                    ("(lambda x:Unit.x)", pass "(lambda x.x):(Unit -> Unit)"),
                    ("(lambda x:Float.x)", pass "(lambda x.x):(Float -> Float)"),
                    ("(lambda x:Bool.lambda y:String.x)", pass "(lambda x.(lambda y.x)):(Bool -> (String -> Bool))"),
                    ("(lambda x:Bool.lambda y:A.x)", pass "(lambda x.(lambda y.x)):(Bool -> (A -> Bool))"),
                    ("(lambda x:Bool.lambda y:A.y)", pass "(lambda x.(lambda y.y)):(Bool -> (A -> A))"),
                    ("(lambda x:(Bool -> A).lambda y:A.y)", pass "(lambda x.(lambda y.y)):((Bool -> A) -> (A -> A))"),
                    ("(lambda x:A -> A.lambda y:A.x y)", pass "(lambda x.(lambda y.x y)):((A -> A) -> (A -> A))"),
                    ("(lambda x:A.x)", pass "(lambda x.x):(A -> A)")
                 ]

    describe "operations" $ do
        describe "condition" $ do
           tests evalString [
                  ("if true then unit else unit", pass "unit:Unit"),
                  ("if false then \"foo\" else \"bar\"", pass "\"bar\":String"),
                  ("if true then 3.14 else 9.8", pass "3.14:Float")
                ]

        describe "predefined functions" $ do
           tests evalString [
                  ("succ zero", pass "succ zero:Nat"),
                  ("pred zero", pass "zero:Nat"),
                  ("pred succ 0", pass "zero:Nat"),
                  ("succ pred pred zero", pass "succ zero:Nat"),
                  ("zero? zero", pass "true:Bool"),
                  ("zero? succ zero", pass "false:Bool"),
                  ("timesfloat 1.0 2.0", pass "2.0:Float"),
                  ("timesfloat (timesfloat 1.3 1.111) 2.0", pass "2.8886000000000003:Float"),
                  ("timesfloat (timesfloat 3.14 2.0) 10.0", pass "62.800000000000004:Float"),
                  ("timesfloat (timesfloat 3.14 2.0) (timesfloat 5.0 2.0)", pass "62.800000000000004:Float")
                 ]

        describe "apply" $ do
           tests evalString [
                  ("(lambda x:Bool. if x then false else true) true", pass "false:Bool"),
                  ("(lambda x:Nat. succ x) zero", pass "succ zero:Nat"),
                  ("(lambda x:Bool -> Bool.lambda y:A. x true)", pass "(lambda x.(lambda y.x true)):((Bool -> Bool) -> (A -> Bool))"),
                  ("(lambda x:Nat. succ x) 1", pass "succ succ zero:Nat"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) pred zero", pass "(lambda y.(lambda z.if zero? zero then y else z)):(Float -> (Float -> Float))"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) zero 3.14 9.8", pass "3.14:Float"),
                  ("(lambda x:Nat -> Nat. x zero) (lambda x:Nat. succ x)", pass "succ zero:Nat"),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14",
                    pass "(lambda p.if (lambda x.zero? x) succ zero then 3.14 else p):(Float -> Float)"
                  ),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14 9.8", pass "9.8:Float"),
                  ("(lambda x:{a:Bool,b:Bool}.x) {a=true, b=false}", pass "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("(lambda x:{a:Bool,b:Bool}.if x.a then false else true) {a=true, b=false}", pass "false:Bool")
                 ]

        describe "fix" $ do
           tests evalString [
                  ("let diverge = (lambda u:Unit.fix (lambda x:T.x)) in diverge", pass "(lambda u.(lambda x.x)):(Unit -> T)"),
                  (
                    "let ff = (lambda ie:Nat -> Bool.lambda x:Nat.if zero? x then true else (if zero? (pred x) then false else ie (pred pred x))) in let iseven = fix ff in iseven",
                    pass "(lambda x.if zero? x then true else if zero? pred x then false\n                                    else (lambda ie.(lambda x'.if zero? x'\n                                                               then true\n                                                               else if zero? pred x'\n                                                                    then false\n                                                                    else ie pred pred x')) pred pred x):(Nat -> Bool)"
                  )
                 ]

        describe "type binding" $ do
           tests evalString [
                    (
                      "T = Nat->Nat",
                      pass ""
                    ),
                    (
                      "Bit = Bool; (lambda x:Bit.x)",
                      pass "(lambda x.x):(Bit -> Bit)"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda x:T.x zero) (lambda y:Nat. if zero? y then succ y else y)",
                      pass "succ zero:Nat"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda f:T. (lambda x:Nat. f x))",
                      pass "(lambda f.(lambda x.f x)):(T -> (Nat -> Nat))"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda f:T. (lambda x:Nat. f (f x)))",
                      pass "(lambda f.(lambda x.f f x)):(T -> (Nat -> Nat))"
                    )
                 ]

        describe "Church bools" $ do
           let definitions = "B = Nat->Nat; \
               \let tru = (lambda t:B.(lambda f:B. t)) in \
               \let fls = (lambda t:B.(lambda f:B. f))"

           tests evalString [
                    (
                      definitions ++ " in fls",
                      pass "(lambda t.(lambda f.f)):(B -> (B -> B))"
                    ),
                    (
                      definitions ++ " in tru",
                      pass "(lambda t.(lambda f.t)):(B -> (B -> B))"
                    )
                 ]

        describe "Church pairs" $ do
           let definitions = "Pair = Bool->Nat; \
                \let pair = (lambda f:Nat.(lambda s:Nat.(lambda i:Bool.if i then f else s))) in \
                \let first = (lambda p:Pair.p true) in \
                \let second = (lambda p:Pair.p false)"

           tests evalString [
                    (
                      definitions ++ "in pair (zero) (succ zero)",
                      pass "(lambda i.if i then zero else succ zero):(Bool -> Nat)"
                    ),
                    (
                      definitions ++ "in let x = pair (zero) (succ zero) in first x",
                      pass "zero:Nat"
                    ),
                    (
                      definitions ++ "in let x = pair (zero) (succ zero) in second x",
                      pass "succ zero:Nat"
                    )
                 ]

        describe "Poly" $ do
           tests evalString [
                    (
                      "(lambda X.lambda x:X. x)",
                      pass "(lambda X.(lambda x.x)):(All X. (X -> X))"
                    ),
                    (
                      "(lambda X.(lambda x:X. x)) [All X.X->T]",
                      pass "(lambda x.x):(X -> X)"
                    ),
                    (
                      "(lambda X.(lambda x:X. x)) [Bool]",
                      pass "(lambda x.x):(X -> X)" -- why not Bool -> Bool ?
                    ),
                    (
                      "{*All Y.Y, (lambda x:(All Y.Y). x)} as {Some X,X->X}",
                      pass "{*(All Y. Y), (lambda x.x) as {Some X, (X -> X)}}:{Some X, (X -> X)}"
                    ),
                    (
                      "{*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}}",
                      pass "{*Nat, {c=zero, f=(lambda x.succ x)} as {Some X, {c=X, f=(X -> Nat)}}}:{Some X, {c=X, f=(X -> Nat)}}"
                    ),
                    (
                      "let {X,ops} = {*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}} in (ops.f ops.c)",
                      pass "succ zero:Nat"
                    ),
                    (
                      "let {X,ops} = {*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}} in ops.f",
                      pass "(lambda x.succ x):(Nat -> Nat)"
                    ),
                    (
                      "let {X,ops} = {*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}} in ops.c",
                      pass "zero:Nat"
                    )
                 ]
