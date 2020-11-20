module Language.TAPL.FullEquirecSpec where

import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FullEquirec.Evaluator (evalString)

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
                  ("0", pass "zero:Nat"),
                  ("5", pass "succ succ succ succ succ zero:Nat"),
                  ("pred 5", pass "succ succ succ succ zero:Nat"),
                  ("4", pass "succ succ succ succ zero:Nat"),
                  ("1.1", pass "1.1:Float"),
                  ("1.1000001", pass "1.1000001:Float")
                ]

        describe "pairs" $ do
            tests evalString [
                  ("{true, false}", pass "{true,false}:{Bool*Bool}"),
                  ("{true, unit}", pass "{true,unit}:{Bool*Unit}"),
                  ("{1.1, \"foo\"}", pass "{1.1,\"foo\"}:{Float*String}"),
                  (
                    "{(lambda x:Unit.x), (lambda x:Unit.x)}",
                    pass "{(lambda x.x),(lambda x.x)}:{(Unit -> Unit)*(Unit -> Unit)}"
                  ),
                  ("{true, false}.0", pass "true:Bool"),
                  ("{true, unit}.1", pass "unit:Unit"),
                  ("{1.1, \"foo\"}.0", pass "1.1:Float"),
                  ("{1.1, \"foo\"}.1", pass "\"foo\":String"),
                  ("{(lambda x:Unit.x), (lambda x:Unit.x)}.1", pass "(lambda x.x):(Unit -> Unit)")
                ]

        describe "records" $ do
            tests evalString [
                  ("{a=true, b=false}", pass "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("{a=true, b=unit}", pass "{a=true, b=unit}:{a=Bool, b=Unit}"),
                  ("{a=1.1, b=\"foo\"}", pass "{a=1.1, b=\"foo\"}:{a=Float, b=String}"),
                  (
                    "{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}",
                    pass "{c=(lambda x.x), d=(lambda x.x)}:{c=(Unit -> Unit), d=(Unit -> Unit)}"
                  ),
                  ("{a=true, b=false}.a", pass "true:Bool"),
                  ("{a=true, b=unit}.b", pass "unit:Unit"),
                  ("{a=1.1, b=\"foo\"}.b", pass "\"foo\":String"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d", pass "(lambda x.x):(Unit -> Unit)")
                ]

        describe "variants" $ do
            tests evalString [
                ("<a=unit> as <a:Unit,b:Unit,c:Nat>", pass "<a=unit>:<a:Unit, b:Unit, c:Nat>"),
                (
                    "(lambda x:<a:Unit,b:Unit,c:Nat>.x)",
                    pass "(lambda x.x):(<a:Unit, b:Unit, c:Nat> -> <a:Unit, b:Unit, c:Nat>)"
                ),
                (
                    "let z = <a=true> as <a:Bool,b:Unit,c:Nat> in (lambda x:<a:Bool, b:Unit, c:Nat>.x) z",
                    pass "<a=true>:<a:Bool, b:Unit, c:Nat>"
                ),
                (
                    "case <b=0> as <a:Nat,b:Nat> of <b=x> -> succ succ pred pred pred x | <a=y> -> zero",
                    pass "succ succ zero:Nat"
                ),
                (
                    "case <b=zero> as <a:Nat,b:Nat> of <b=x> -> if (zero? x) then false else true | <a=y> -> true",
                    pass "false:Bool"
                ),
                (
                    "case <b=succ zero> as <a:Nat,b:Nat> of <a=x> -> <a=succ x> as <a:Nat>| <b=y> -> <a=succ y> as <a:Nat>",
                    pass "<a=succ succ zero>:<a:Nat>"
                )
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
                    (
                      "(lambda x:<a:Bool,b:Bool>. x)",
                      pass "(lambda x.x):(<a:Bool, b:Bool> -> <a:Bool, b:Bool>)"
                    ),
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
                    ("(lambda x:Top.x)", pass "(lambda x.x):(Top -> Top)"),
                    ("(lambda x:Bot.x)", pass "(lambda x.x):(Bot -> Bot)"),
                    ("(lambda x:Bot.x)", pass "(lambda x.x):(Bot -> Bot)"),
                    ("(lambda x:{Bot*Top}.x.1)", pass "(lambda x.x.1):({Bot*Top} -> Top)"),
                    ("(lambda x:{Bool*{Unit*Top}}.true)", pass "(lambda x.true):({Bool*{Unit*Top}} -> Bool)"),
                    (
                        "(lambda x:{Bool*{Unit*(A->B)}}.x)",
                        pass "(lambda x.x):({Bool*{Unit*(A -> B)}} -> {Bool*{Unit*(A -> B)}})"
                    ),
                    (
                        "(lambda x:{Bool*{Unit*{Unit*Float}}}.x)",
                        pass "(lambda x.x):({Bool*{Unit*{Unit*Float}}} -> {Bool*{Unit*{Unit*Float}}})"),
                    (
                        "(lambda f:Rec X.A->A.lambda x:A. f x)",
                        pass "(lambda f.(lambda x.f x)):(Rec X.(A -> A) -> (A -> A))"),
                    (
                        "(lambda x:Rec P.{get:Nat, inc:Unit->P}.x)",
                        pass "(lambda x.x):(Rec P.{get=Nat, inc=(Unit -> P)} -> Rec P.{get=Nat, inc=(Unit -> P)})")
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
                  (
                    "(lambda x:Bool -> Bool.lambda y:A. x true)",
                    pass "(lambda x.(lambda y.x true)):((Bool -> Bool) -> (A -> Bool))"
                  ),
                  ("(lambda x:Nat. succ x) succ zero", pass "succ succ zero:Nat"),
                  (
                    "(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) pred zero",
                    pass "(lambda y.(lambda z.if zero? zero then y else z)):(Float -> (Float -> Float))"
                  ),
                  (
                    "(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) zero 3.14 9.8",
                    pass "3.14:Float"
                  ),
                  ("(lambda x:Nat -> Nat. x zero) (lambda x:Nat. succ x)", pass "succ zero:Nat"),
                  (
                    "(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then false else true);",
                    pass "true:Bool"
                  ),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14",
                    pass "(lambda p.if (lambda x.zero? x) succ zero then 3.14 else p):(Float -> Float)"),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14 9.8", pass "9.8:Float"),
                  ("(lambda x:{a:Bool,b:Bool}.x) {a=true, b=false}", pass "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("(lambda x:{a:Bool,b:Bool}.if x.a then false else true) {a=true, b=false}", pass "false:Bool"),
                  ("(lambda x:<a:Unit,b:Unit,c:Nat>.x) <a=unit> as <a:Unit,b:Unit,c:Nat>", pass "<a=unit>:<a:Unit, b:Unit, c:Nat>")
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
                      "Bit = Bool; (lambda x:Bit.x) true",
                      pass "true:Bit"
                    ),
                    (
                      "T = Nat->Nat",
                      pass ""
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda x:T.x zero) (lambda y:Nat. if zero? y then succ y else y)",
                      pass "succ zero:Nat"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda f:T.(lambda x:Nat. f (f x)))",
                      pass "(lambda f.(lambda x.f f x)):(T -> (Nat -> Nat))"
                    ),
                    (
                        "Counter = Rec P.{get:Nat, inc:Unit->P}; let counter = fix (lambda cr:{x:Nat}->Counter.(lambda s:{x:Nat}.{get=s.x, inc=(lambda b:Unit. cr {x=succ(s.x)})})) in counter",
                        pass "(lambda s.{get=s.x, inc=(lambda b.(lambda cr.(lambda s'.{get=s'.x, inc=(lambda b'.cr {x=succ s'.x})})) {x=succ s.x})}):({x=Nat} -> {get=Nat, inc=(Unit -> Counter)})"
                    ),
                    (
                      "Counter = Rec P.{get:Nat, inc:Unit->P}; \
                      \ let create = fix (lambda cr:{x:Nat}->Counter.(lambda s:{x:Nat}.{get=s.x, inc=(lambda b:Unit. cr {x=succ(s.x)})})) in \
                      \ let counter = create {x=zero} in \
                      \ let get = (lambda p:Counter. p.get) in \
                      \ let inc = (lambda p:Counter. p.inc) in \
                      \ let counter2 = (inc counter) unit in \
                      \ let z = (get counter2) in \
                      \ z",
                      pass "succ zero:Nat"
                    ),
                    (
                      "Hungry = Rec A. Nat -> A; \
                      \ let f0 = fix (lambda f:Nat->Hungry.(lambda n:Nat.f)) in f0;",
                      pass "(lambda n.(lambda f.(lambda n'.f))):(Nat -> (Nat -> Hungry))"
                    ),
                    (
                      "Hungry = Rec A. Nat -> A; \
                      \ let f0 = fix (lambda f:Nat->Hungry.(lambda n:Nat.f)) in \
                      \ let f1 = f0 zero in \
                      \ let f2 = f1 (succ succ zero) in \
                      \ f2;",
                      pass "(lambda n.(lambda f.(lambda n'.f))):Hungry"
                    ),
                    (
                      "T = Nat; \
                      \ let fix_T = (lambda f:T->T.(lambda x:Rec A.A->T. f (x x)) (lambda x:Rec A.A->T. f (x x))) in \
                      \ fix_T",
                      pass "(lambda f.(lambda x.f x x) (lambda x.f x x)):((T -> T) -> T)"
                    ),
                    (
                      "D = Rec X. X->X; \
                      \ let fix_D = (lambda f:D->D.(lambda x:Rec A.A->D. f (x x)) (lambda x:Rec A.A->D. f (x x))) in \
                      \ fix_D;",
                      pass "(lambda f.(lambda x.f x x) (lambda x.f x x)):((D -> D) -> D)"
                    ),
                    (
                      "D = Rec X. X->X; \
                      \ let fix_D = (lambda f:D->D.(lambda x:Rec A.A->D. f (x x)) (lambda x:Rec A.A->D. f (x x))) in \
                      \ let diverge_D = (lambda g:Unit. fix_D (lambda x:D. x)) in \
                      \ diverge_D",
                      pass "(lambda g.(lambda f.(lambda x.f x x) (lambda x.f x x)) (lambda x.x)):(Unit -> D)"
                    ),
                    (
                      "D = Rec X. X->X; \
                      \let lam = (lambda f:D->D. f) in \
                      \let ap = (lambda f:D.(lambda a:D. f a)) in \
                      \let myfix = lam (lambda f:D. ap (lam (lambda x:D. ap f (ap x x))) (lam (lambda x:D. ap f (ap x x)))) in \
                      \myfix",
                      pass "(lambda f.(lambda f'.(lambda a.f' a)) (lambda f'.f') (lambda x.(lambda f'.(lambda a.f' a)) f (lambda f'.(lambda a.f' a)) x x) (lambda f'.f') (lambda x.(lambda f'.(lambda a.f' a)) f (lambda f'.(lambda a.f' a)) x x)):(D -> D)"
                    ),
                    (
                      "Natlist = Rec X. <nil:Unit, cons:{Nat*X}>; \
                      \let nil = <nil=unit> as Natlist in \
                      \let cons = (lambda n:Nat.(lambda l:Natlist. <cons={n,l}> as Natlist)) in \
                      \let isnil = (lambda l:Natlist. case l of <nil=u> -> true | <cons=p> -> false) in \
                      \let hd = (lambda l:Natlist. case l of <nil=u> -> zero | <cons=p> -> p.0) in \
                      \let tl = (lambda l:Natlist. case l of <nil=u> -> l | <cons=p> -> p.1) in \
                      \let plus = fix (lambda p:Nat->Nat->Nat.(lambda m:Nat.(lambda n:Nat. if zero? m then n else succ (p (pred m) n)))) in \
                      \let sumlist = fix (lambda s:Natlist->Nat.(lambda l:Natlist. if isnil l then zero else plus (hd l) (s (tl l)))) in \
                      \let mylist = cons (succ succ zero) (cons (succ succ succ zero) (cons (succ succ succ succ succ zero) nil)) in \
                      \let sum = sumlist mylist in \
                      \{mylist, sum}",
                      pass "{<cons={succ succ zero,<cons={succ succ succ zero,<cons={succ succ succ succ succ zero,<nil=unit>}>}>}>,succ succ succ succ succ succ succ succ succ succ zero}:{Natlist*Nat}"
                    )
                 ]

