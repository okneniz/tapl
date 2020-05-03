{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullEquirecSpec where

import Test.Hspec
import Language.TAPL.FullEquirec.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "values" $ do
        describe "primitive values" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("true", "true:Bool"),
                  ("false", "false:Bool"),
                  ("\"foo\"", "\"foo\":String"),
                  ("unit", "unit:Unit"),
                  ("1.1", "1.1:Float"),
                  ("1.1000001", "1.1000001:Float")
                ]
           mapM_ test examples

        describe "pairs" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("{true, false}", "{true,false}:{Bool*Bool}"),
                  ("{true, unit}", "{true,unit}:{Bool*Unit}"),
                  ("{1.1, \"foo\"}", "{1.1,\"foo\"}:{Float*String}"),
                  ("{(lambda x:Unit.x), (lambda x:Unit.x)}", "{(lambda x.x),(lambda x.x)}:{(Unit -> Unit)*(Unit -> Unit)}"),
                  ("{true, false}.0", "true:Bool"),
                  ("{true, unit}.1", "unit:Unit"),
                  ("{1.1, \"foo\"}.0", "1.1:Float"),
                  ("{1.1, \"foo\"}.1", "\"foo\":String"),
                  ("{(lambda x:Unit.x), (lambda x:Unit.x)}.1", "(lambda x.x):(Unit -> Unit)")
                ]
           mapM_ test examples

        describe "records" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("{a=true, b=false}", "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("{a=true, b=unit}", "{a=true, b=unit}:{a=Bool, b=Unit}"),
                  ("{a=1.1, b=\"foo\"}", "{a=1.1, b=\"foo\"}:{a=Float, b=String}"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}", "{c=(lambda x.x), d=(lambda x.x)}:{c=(Unit -> Unit), d=(Unit -> Unit)}"),
                  ("{a=true, b=false}.a", "true:Bool"),
                  ("{a=true, b=unit}.b", "unit:Unit"),
                  ("{a=1.1, b=\"foo\"}.b", "\"foo\":String"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d", "(lambda x.x):(Unit -> Unit)")
                ]
           mapM_ test examples

        describe "variants" $ do
          let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
              examples = [
                ("<a=unit> as <a:Unit,b:Unit,c:Nat>", "<a=unit>:<a:Unit, b:Unit, c:Nat>"),
                ("(lambda x:<a:Unit,b:Unit,c:Nat>.x)", "(lambda x.x):(<a:Unit, b:Unit, c:Nat> -> <a:Unit, b:Unit, c:Nat>)"),
                ("let z = <a=true> as <a:Bool,b:Unit,c:Nat> in (lambda x:<a:Bool, b:Unit, c:Nat>.x) z", "<a=true>:<a:Bool, b:Unit, c:Nat>"),
                ("case <b=zero> as <a:Int,b:Nat> of <b=x> -> succ succ pred pred pred x | <a=y> -> zero", "succ succ zero:Nat"),
                ("case <b=zero> as <a:Nat,b:Nat> of <b=x> -> if (zero? x) then false else true | <a=y> -> true", "false:Bool"),
                ("case <b=succ zero> as <a:Nat,b:Nat> of <a=x> -> <a=succ x> as <a:Nat>| <b=y> -> <a=succ y> as <a:Nat>", "<a=succ succ zero>:<a:Nat>")
               ]
          mapM_ test examples

        describe "ascribe" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("\"foo\" as String", "\"foo\":String"),
                  ("false as Bool", "false:Bool"),
                  ("1.1 as Float", "1.1:Float"),
                  ("true as Bool", "true:Bool"),
                  ("unit as Unit", "unit:Unit"),
                  ("1.1000001 as Float", "1.1000001:Float")
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
                    (
                      "(lambda x:<a:Bool,b:Bool>. x)",
                      "(lambda x.x):(<a:Bool, b:Bool> -> <a:Bool, b:Bool>)"
                    ),
                    ("(lambda x:Bool.x)", "(lambda x.x):(Bool -> Bool)"),
                    ("(lambda x:String.x)", "(lambda x.x):(String -> String)"),
                    ("(lambda x:Unit.x)", "(lambda x.x):(Unit -> Unit)"),
                    ("(lambda x:Float.x)", "(lambda x.x):(Float -> Float)"),
                    ("(lambda x:Bool.lambda y:String.x)", "(lambda x.(lambda y.x)):(Bool -> (String -> Bool))"),
                    ("(lambda x:Bool.lambda y:A.x)", "(lambda x.(lambda y.x)):(Bool -> (A -> Bool))"),
                    ("(lambda x:Bool.lambda y:A.y)", "(lambda x.(lambda y.y)):(Bool -> (A -> A))"),
                    ("(lambda x:(Bool -> A).lambda y:A.y)", "(lambda x.(lambda y.y)):((Bool -> A) -> (A -> A))"),
                    ("(lambda x:A -> A.lambda y:A.x y)", "(lambda x.(lambda y.x y)):((A -> A) -> (A -> A))"),
                    ("(lambda x:A.x)", "(lambda x.x):(A -> A)"),
                    ("(lambda x:Top.x)", "(lambda x.x):(Top -> Top)"),
                    ("(lambda x:Bot.x)", "(lambda x.x):(Bot -> Bot)"),
                    ("(lambda x:Bot.x)", "(lambda x.x):(Bot -> Bot)"),
                    ("(lambda x:{Bot*Top}.x.1)", "(lambda x.x.1):({Bot*Top} -> Top)"),
                    ("(lambda x:{Bool*{Unit*Top}}.true)", "(lambda x.true):({Bool*{Unit*Top}} -> Bool)"),
                    ("(lambda x:{Bool*{Unit*(A->B)}}.x)", "(lambda x.x):({Bool*{Unit*(A -> B)}} -> {Bool*{Unit*(A -> B)}})"),
                    ("(lambda x:{Bool*{Unit*{Unit*Float}}}.x)", "(lambda x.x):({Bool*{Unit*{Unit*Float}}} -> {Bool*{Unit*{Unit*Float}}})"),
                    ("(lambda f:Rec X.A->A.lambda x:A. f x)", "(lambda f.(lambda x.f x)):(Rec X.(A -> A) -> (A -> A))"),
                    ("(lambda x:Rec P.{get:Nat, inc:Unit->P}.x)", "(lambda x.x):(Rec P.{get=Nat, inc=(Unit -> P)} -> Rec P.{get=Nat, inc=(Unit -> P)})")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "condition" $ do
           let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("if true then unit else unit", "unit:Unit"),
                  ("if false then \"foo\" else \"bar\"", "\"bar\":String"),
                  ("if true then 3.14 else 9.8", "3.14:Float")
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
                  ("zero? succ zero", "false:Bool"),
                  ("timesfloat 1.0 2.0", "2.0:Float"),
                  ("timesfloat (timesfloat 1.3 1.111) 2.0", "2.8886000000000003:Float"),
                  ("timesfloat (timesfloat 3.14 2.0) 10.0", "62.800000000000004:Float")
                 ]
            mapM_ test examples

        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bool. if x then false else true) true", "false:Bool"),
                  ("(lambda x:Nat. succ x) zero", "succ zero:Nat"),
                  ("(lambda x:Bool -> Bool.lambda y:A. x true)", "(lambda x.(lambda y.x true)):((Bool -> Bool) -> (A -> Bool))"),
                  ("(lambda x:Nat. succ x) succ zero", "succ succ zero:Nat"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) pred zero", "(lambda y.(lambda z.if zero? zero then y else z)):(Float -> (Float -> Float))"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) zero 3.14 9.8", "3.14:Float"),
                  ("(lambda x:Nat -> Nat. x zero) (lambda x:Nat. succ x)", "succ zero:Nat"),
                  (
                    "(lambda x:Bool->Bool. if x false then true else false) (lambda x:Bool. if x then false else true);",
                    "true:Bool"
                  ),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14", "(lambda p.if (lambda x.zero? x) succ zero then 3.14 else p):(Float -> Float)"),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14 9.8", "9.8:Float"),
                  ("(lambda x:{a:Bool,b:Bool}.x) {a=true, b=false}", "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("(lambda x:{a:Bool,b:Bool}.if x.a then false else true) {a=true, b=false}", "false:Bool"),
                  ("(lambda x:<a:Unit,b:Unit,c:Nat>.x) <a=unit> as <a:Unit,b:Unit,c:Nat>", "<a=unit>:<a:Unit, b:Unit, c:Nat>")
                 ]
            mapM_ test examples

        describe "fix" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("let diverge = (lambda u:Unit.fix (lambda x:T.x)) in diverge", "(lambda u.(lambda x.x)):(Unit -> T)"),
                  (
                    "let ff = (lambda ie:Nat -> Bool.lambda x:Nat.if zero? x then true else (if zero? (pred x) then false else ie (pred pred x))) in let iseven = fix ff in iseven",
                    "(lambda x.if zero? x then true else if zero? pred x then false\n                                    else (lambda ie.(lambda x'.if zero? x'\n                                                               then true\n                                                               else if zero? pred x'\n                                                                    then false\n                                                                    else ie pred pred x')) pred pred x):(Nat -> Bool)"

                  )
                 ]
            mapM_ test examples

        describe "type binding" $ do
            let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
                examples = [
                    (
                      "Bit = Bool; (lambda x:Bit.x) true",
                      "true:Bool"
                    ),
                    (
                      "T = Nat->Nat",
                      ""
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda x:T.x zero) (lambda y:Nat. if zero? y then succ y else y)",
                      "succ zero:Nat"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda f:T.(lambda x:Nat. f (f x)))",
                      "(lambda f.(lambda x.f f x)):(T -> (Nat -> Nat))"
                    ),
                    (
                        "Counter = Rec P.{get:Nat, inc:Unit->P}; let counter = fix (lambda cr:{x:Nat}->Counter.(lambda s:{x:Nat}.{get=s.x, inc=(lambda b:Unit. cr {x=succ(s.x)})})) in counter",
                        "(lambda s.{get=s.x, inc=(lambda b.(lambda cr.(lambda s'.{get=s'.x, inc=(lambda b'.cr {x=succ s'.x})})) {x=succ s.x})}):({x=Nat} -> {get=Nat, inc=(Unit -> {get=Nat, inc=(Unit -> Counter)})})"
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
                      "succ zero:Nat"
                    ),
                    (
                      "Hungry = Rec A. Nat -> A; \
                      \ let f0 = fix (lambda f:Nat->Hungry.(lambda n:Nat.f)) in f0;",
                      "(lambda n.(lambda f.(lambda n'.f))):(Nat -> (Nat -> (Nat -> Hungry)))"
                    ),
                    (
                      "Hungry = Rec A. Nat -> A; \
                      \ let f0 = fix (lambda f:Nat->Hungry.(lambda n:Nat.f)) in \
                      \ let f1 = f0 zero in \
                      \ let f2 = f1 (succ succ zero) in \
                      \ f2;",
                      "(lambda n.(lambda f.(lambda n'.f))):(Nat -> (Nat -> (Nat -> Hungry)))"
                    ),
                    (
                      "T = Nat; \
                      \ let fix_T = (lambda f:T->T.(lambda x:Rec A.A->T. f (x x)) (lambda x:Rec A.A->T. f (x x))) in \
                      \ fix_T",
                      "(lambda f.(lambda x.f x x) (lambda x.f x x)):((T -> T) -> T)"
                    ),
                    (
                      "D = Rec X. X->X; \
                      \ let fix_D = (lambda f:D->D.(lambda x:Rec A.A->D. f (x x)) (lambda x:Rec A.A->D. f (x x))) in \
                      \ fix_D;",
                      "(lambda f.(lambda x.f x x) (lambda x.f x x)):((D -> D) -> D)"
                    ),
                    (
                      "D = Rec X. X->X; \
                      \ let fix_D = (lambda f:D->D.(lambda x:Rec A.A->D. f (x x)) (lambda x:Rec A.A->D. f (x x))) in \
                      \ let diverge_D = (lambda g:Unit. fix_D (lambda x:D. x)) in \
                      \ diverge_D",
                      "(lambda g.(lambda f.(lambda x.f x x) (lambda x.f x x)) (lambda x.x)):(Unit -> D)"
                    ),
                    (
                      "D = Rec X. X->X; \
                      \let lam = (lambda f:D->D. f) in \
                      \let ap = (lambda f:D.(lambda a:D. f a)) in \
                      \let myfix = lam (lambda f:D. ap (lam (lambda x:D. ap f (ap x x))) (lam (lambda x:D. ap f (ap x x)))) in \
                      \myfix",
                      "(lambda f.(lambda f'.(lambda a.f' a)) (lambda f'.f') (lambda x.(lambda f'.(lambda a.f' a)) f (lambda f'.(lambda a.f' a)) x x) (lambda f'.f') (lambda x.(lambda f'.(lambda a.f' a)) f (lambda f'.(lambda a.f' a)) x x)):(D -> Rec X.(X -> X))"
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
                      "{<cons={succ succ zero,<cons={succ succ succ zero,<cons={succ succ succ succ succ zero,<nil=unit>}>}>}>,succ succ succ succ succ succ succ succ succ succ zero}:{Natlist*Nat}"
                    )
                 ]
            mapM_ test examples
