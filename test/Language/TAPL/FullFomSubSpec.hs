module Language.TAPL.FullFomSubSpec where
import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FullFomSub.Evaluator (evalString)

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
              ("1.1000001", pass "1.1000001:Float")
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
                    ("(lambda x:A.x)", pass "(lambda x.x):(A -> A)"),
                    ("lambda x:Top. x", pass "(lambda x.x):(Top -> Top)")
                 ]

    describe "operations" $ do
        describe "condition" $ do
           tests evalString [
                  ("if true then unit else unit", pass "unit:Unit"),
                  ("if false then \"foo\" else \"bar\"", pass "\"bar\":String"),
                  ("if true then 3.14 else 9.8", pass "3.14:Float"),
                  (
                    "if true then {x=true,y=false,a=false} else {y=false,x={},b=false}",
                    pass "{a=false, x=true, y=false}:{x=Top, y=Bool}"
                  )
                  -- TODO : check behaviour in ocaml implementations
--                  (
--                    "if false then {x=true,y=false,a=false} else {y=false,x={},b=false}",
--                    pass "{a=false, x=true, y=false}:{a=Bool, x=Bool, y=Bool}"
--                  )
                ]

        describe "predefined functions" $ do
           tests evalString [
                  ("succ zero", pass "succ zero:Nat"),
                  ("pred zero", pass "zero:Nat"),
                  ("pred succ zero", pass "zero:Nat"),
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
                  ("(lambda x:Nat. succ x) succ zero", pass "succ succ zero:Nat"),
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
                  ("(lambda x:{a:Bool,b:Bool}.if x.a then false else true) {a=true, b=false}", pass "false:Bool"),
                  ("(lambda x:Top. x) (lambda x:Top. x)", pass "(lambda x.x):Top"),
                  ("(lambda x:Top->Top. x) (lambda x:Top. x)", pass  "(lambda x.x):(Top -> Top)"),
                  (
                    "(lambda r:{x:Top->Top}. r.x r.x) {x=lambda z:Top.z, y=lambda z:Top.z}",
                    pass "(lambda z.z):Top"
                  ),
                  (
                    "(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then false else true)",
                    pass "true:Bool"
                  ),
                  (
                    "(lambda x:Nat. succ x)",
                    pass "(lambda x.succ x):(Nat -> Nat)"
                  ),
                  (
                    "(lambda x:Nat. succ (succ x)) (succ zero)",
                    pass "succ succ succ zero:Nat"
                  )
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
               \let fls = (lambda t:B.(lambda f:B. f)) in "

           tests evalString [
                    (
                      definitions ++ "fls",
                      pass "(lambda t.(lambda f.f)):(B -> (B -> B))"
                    ),
                    (
                      definitions ++ "tru",
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
                      definitions ++ "in let x = pair zero (succ zero) in first x",
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
                      pass "(lambda X.(lambda x.x)):(All X<:Top. (X -> X))"
                    ),
                    (
                      "(lambda X.(lambda x:X. x)) [All X.X->T]",
                      pass "(lambda x.x):((All X<:Top. (X -> T)) -> (All X<:Top. (X -> T)))"
                    ),
                    (
                      "(lambda X.(lambda x:X. x)) [Bool]",
                      pass "(lambda x.x):(Bool -> Bool)"
                    ),
                    (
                      "{*All Y.Y, lambda x:(All Y.Y). x} as {Some X,X->X}",
                      pass "{*(All Y<:Top. Y), (lambda x.x)}:{Some X<:Top, (X -> X)}"
                    ),
                    (
                      "{*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}}",
                      pass "{*Nat, {c=zero, f=(lambda x.succ x)}}:{Some X<:Top, {c=X, f=(X -> Nat)}}"
                    ),
                    (
                      "let {X,ops} = {*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}} in (ops.f ops.c)",
                      pass "succ zero:Nat"
                    ),
                    (
                      "let {X,ops} = {*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}} in ops.f",
                      failed "\"<stdin>\" (line 1, column 5) : attempt to use type variable in invalid scope"
                    ),
                    (
                      "let {X,ops} = {*Nat, {c=zero, f=(lambda x:Nat. succ x)}} as {Some X, {c:X, f:X->Nat}} in ops.c",
                      failed "\"<stdin>\" (line 1, column 5) : attempt to use type variable in invalid scope"
                    )
                 ]

        describe "Omega" $ do
            describe "Pair" $ do
                let definitions = "\
                \Pair = (lambda X.lambda Y. All R. (X->Y->R) -> R);  \
                \let f = (lambda X.lambda Y.lambda f:Pair X Y. f) in \
                \let fst = lambda X.lambda Y.lambda p:Pair X Y.(p [X]) (lambda x:X.lambda y:Y.x) in \
                \let snd = lambda X.lambda Y.lambda p:Pair X Y.(p [Y]) (lambda x:X.lambda y:Y.y) in \
                \let pair = lambda X.lambda Y.lambda x:X.lambda y:Y.lambda R.lambda p:X->Y->R.(p x) y in "

                tests evalString [
                    (
                      definitions ++ "f",
                      pass "(lambda X.(lambda Y.(lambda f.f))):(All X<:Top. (All Y<:Top. (Pair X Y -> Pair X Y)))"
                    ),
                    (
                      definitions ++ "fst [Nat] [Bool]",
                      pass "(lambda p.p Nat (lambda x.(lambda y.x))):(Pair Nat Bool -> Nat)"
                    ),
                    (
                      definitions ++ "snd [Nat] [Bool]",
                      pass "(lambda p.p Bool (lambda x.(lambda y.y))):(Pair Nat Bool -> Bool)"
                    ),
                    (
                      definitions ++ "pair",
                      pass "(lambda X.(lambda Y.(lambda x.(lambda y.(lambda R.(lambda p.p x y)))))):(All X<:Top. (All Y<:Top. (X -> (Y -> (All R<:Top. ((X -> (Y -> R)) -> R))))))"
                    ),
                    (
                      definitions ++ "pair [Nat] [Bool] zero false",
                      pass "(lambda R.(lambda p.p zero false)):(All R<:Top. ((Nat -> (Bool -> R)) -> R))"
                    ),
                    (
                      definitions ++ "\
                      \let pr = pair [Nat] [Bool] zero false in \
                      \(fst [Nat] [Bool]) pr",
                      pass "zero:Nat"
                    ),
                    (
                      definitions ++ "\
                      \let pr = pair [Nat] [Bool] zero false in \
                      \(snd [Nat] [Bool]) pr",
                      pass "false:Bool"
                    )
                 ]

            describe "List" $ do
                let definitions = "\
                \List = lambda X. All R. (X->R->R) -> R -> R;\
                \Pair = (lambda X.lambda Y. All R. (X->Y->R) -> R);  \
                \let f = (lambda X.lambda Y.lambda f:Pair X Y. f) in \
                \let fst = lambda X.lambda Y.lambda p:Pair X Y.(p [X]) (lambda x:X.lambda y:Y.x) in \
                \let snd = lambda X.lambda Y.lambda p:Pair X Y.(p [Y]) (lambda x:X.lambda y:Y.y) in \
                \let pair = lambda X.lambda Y.lambda x:X.lambda y:Y.lambda R.lambda p:X->Y->R.(p x) y in \
                \let diverge = (lambda X.lambda x:Unit.fix (lambda x:X. x)) in \
                \let nil = (lambda X.lambda R.lambda c:X->R->R.lambda n:R. n) in \
                \let cons = (lambda X.(lambda hd:X.lambda tl: List X.lambda R.lambda c:X->R->R.lambda n:R. c hd (tl [R] c n))) in \
                \let isnil = lambda X.lambda l: List X.l [Bool] (lambda hd:X.lambda tl:Bool. false) true in \
                \let head = (lambda X.lambda l:List X. \
                \    let d = diverge [X] in \
                \    let z = (lambda hd:X.lambda tl:Unit->X.lambda z:Unit.hd) in \
                \    let r = l [Unit->X] z d in \
                \    r unit \
                \) in \
                \let tail = (lambda X.lambda l: List X. \
                \     (fst [List X] [List X] ( \
                \       l [Pair (List X) (List X)] \
                \         (lambda hd: X. lambda tl: Pair (List X) (List X). \
                \           pair [List X] [List X] \
                \             (snd [List X] [List X] tl) \
                \             (cons [X] hd (snd [List X] [List X] tl))) \
                \         (pair [List X] [List X] (nil [X]) (nil [X])))) \
                \     ) \
                \ in \
                \"

                tests evalString [
                    (
                      definitions ++ "diverge",
                      pass "(lambda X.(lambda x.(lambda x'.x'))):(All X<:Top. (Unit -> X))"
                    ),
                    (
                      definitions ++ "nil",
                      pass "(lambda X.(lambda R.(lambda c.(lambda n.n)))):(All X<:Top. (All R<:Top. ((X -> (R -> R)) -> (R -> R))))"
                    ),
                    (
                      definitions ++ "cons",
                      pass "(lambda X.(lambda hd.(lambda tl.(lambda R.(lambda c.(lambda n.c hd tl R c n)))))):(All X<:Top. (X -> (List X -> (All R<:Top. ((X -> (R -> R)) -> (R -> R))))))"
                    ),
                    (
                      definitions ++ "isnil",
                      pass "(lambda X.(lambda l.l Bool (lambda hd.(lambda tl.false)) true)):(All X<:Top. (List X -> Bool))"
                    ),
                    (
                      definitions ++ "head",
                      pass "(lambda X.(lambda l.let d = (lambda X'.(lambda x.(lambda x'.x'))) X in let z = (lambda hd.(lambda tl.(lambda z.hd))) in let r = Pair (Unit -> List) l X in l unit)):(All X<:Top. (List X -> X))"
                    ),
                    (
                      definitions ++ "tail",
                      pass "(lambda X.(lambda l.(lambda X'.(lambda Y.(lambda p.p X' (lambda x.(lambda y.x))))) List X List X l Pair List X List X (lambda hd.(lambda tl.(lambda X'.(lambda Y.(lambda x.(lambda y.(lambda R.(lambda p.p x y)))))) List X List X (lambda X'.(lambda Y.(lambda p.p Y (lambda x.(lambda y.y))))) List X List X tl (lambda X'.(lambda hd'.(lambda tl'.(lambda R.(lambda c.(lambda n.c hd' tl' R c n)))))) X hd (lambda X'.(lambda Y.(lambda p.p Y (lambda x.(lambda y.y))))) List X List X tl)) (lambda X'.(lambda Y.(lambda x.(lambda y.(lambda R.(lambda p.p x y)))))) List X List X (lambda X'.(lambda R.(lambda c.(lambda n.n)))) X (lambda X'.(lambda R.(lambda c.(lambda n.n)))) X)):(All X<:Top. (List X -> List X))"
                    ),
                    (
                      definitions ++ "cons [Nat]",
                      pass "(lambda hd.(lambda tl.(lambda R.(lambda c.(lambda n.c hd tl R c n))))):(Nat -> (List Nat -> (All R<:Top. ((Nat -> (R -> R)) -> (R -> R)))))"
                    ),
                    (
                      definitions ++ "\
                      \let one = succ zero in \
                      \let two = succ one in \
                      \let three = succ two in \
                      \let c = cons [Nat] in \
                      \let n = nil [Nat] in \
                      \let list = c three (c two (c one (c zero n))) in \
                      \list",
                      pass  "(lambda R.(lambda c.(lambda n.c succ succ succ zero (lambda R'.(lambda c'.(lambda n'.c' succ succ zero (lambda R''.(lambda c''.(lambda n''.c'' succ zero (lambda R'''.(lambda c'''.(lambda n'''.c''' zero (lambda R''''.(lambda c''''.(lambda n''''.n''''))) R''' c''' n'''))) R'' c'' n''))) R' c' n'))) R c n))):(All R<:Top. ((Nat -> (R -> R)) -> (R -> R)))"
                    ),
                    (
                      definitions ++ "\
                      \let one = succ zero in \
                      \let two = succ one in \
                      \let three = succ two in \
                      \let c = cons [Nat] in \
                      \let n = nil [Nat] in \
                      \let list = c three (c two (c one (c zero n))) in \
                      \head [Nat] list",
                      pass  "succ succ succ zero:Nat"
                    ),
                    (
                      definitions ++ "\
                      \let one = succ zero in \
                      \let two = succ one in \
                      \let three = succ two in \
                      \let c = cons [Nat] in \
                      \let n = nil [Nat] in \
                      \let list = c three (c two (c one (c zero n))) in \
                      \isnil [Nat] list",
                      pass  "false:Bool"
                    ),
                    (
                      definitions ++ "isnil [Nat] (nil [Nat])",
                      pass "true:Bool"
                    ),
                    (
                      definitions ++ "\
                      \let one = succ zero in \
                      \let two = succ one in \
                      \let three = succ two in \
                      \let c = cons [Nat] in \
                      \let n = nil [Nat] in \
                      \let list = c three (c two (c one (c zero n))) in \
                      \head [Nat] (tail [Nat] list)",
                      pass  "succ succ zero:Nat"
                    ),
                    (
                      definitions ++ "\
                      \let one = succ zero in \
                      \let two = succ one in \
                      \let three = succ two in \
                      \let c = cons [Nat] in \
                      \let n = nil [Nat] in \
                      \let list = c three (c two (c one (c zero n))) in \
                      \head [Nat] (tail [Nat] (tail [Nat] list))",
                      pass  "succ zero:Nat"
                    ),
                    (
                      definitions ++ "\
                      \let one = succ zero in \
                      \let two = succ one in \
                      \let three = succ two in \
                      \let c = cons [Nat] in \
                      \let n = nil [Nat] in \
                      \let list = c three (c two (c one (c zero n))) in \
                      \head [Nat] (tail [Nat] (tail [Nat] (tail [Nat] list)))",
                      pass  "zero:Nat"
                    )
                 ]
