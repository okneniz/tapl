module Language.TAPL.FullFSubRefSpec where
import Test.Hspec
import Language.TAPL.TestHelpers
import Language.TAPL.FullFSubRef.Evaluator (evalString)

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
                  ("{key1=ref (lambda x:Unit.x), key2=ref ref ref unit}", pass "{key1=<0>, key2=<3>}:{key1=Ref (Unit -> Unit), key2=Ref Ref Ref Unit}"),
                  ("{a=true, b=false}.a", pass "true:Bool"),
                  ("{a=true, b=unit}.b", pass "unit:Unit"),
                  ("{a=1.1, b=\"foo\"}.b", pass "\"foo\":String"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d", pass "(lambda x.x):(Unit -> Unit)"),
                  ("{key1=ref (lambda x:Unit.x), key2=ref ref ref unit}.key2", pass "<3>:Ref Ref Ref Unit"),
                  ("!{key1=ref (lambda x:Unit.x), key2=ref ref ref unit}.key2", pass "<2>:Ref Ref Unit")
                ]

        describe "variants" $ do
            tests evalString [
                ("<a=unit> as <a:Unit,b:Unit,c:Nat>", pass "<a=unit>:<a:Unit, b:Unit, c:Nat>"),
                ("(lambda x:<a:Unit,b:Unit,c:Nat>.x)", pass "(lambda x.x):(<a:Unit, b:Unit, c:Nat> -> <a:Unit, b:Unit, c:Nat>)"),
                ("let z = <a=true> as <a:Bool,b:Unit,c:Nat> in (lambda x:<a:Bool, b:Unit, c:Nat>.x) z", pass "<a=true>:<a:Bool, b:Unit, c:Nat>"),
                ("case <b=zero> as <a:Int,b:Nat> of <b=x> -> succ succ pred pred pred x | <a=y> -> zero", pass "succ succ zero:Nat"),
                ("case <b=zero> as <a:Nat,b:Nat> of <b=x> -> if (zero? x) then false else true | <a=y> -> true", pass "false:Bool"),
                ("case <b=succ zero> as <a:Nat,b:Nat> of <a=x> -> <a=succ x> as <a:Nat>| <b=y> -> <a=succ y> as <a:Nat>", pass "<a=succ succ zero>:<a:Nat>")
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

        describe "references" $ do
            tests evalString [
                  ("ref true", pass "<0>:Ref Bool"),
                  ("ref false", pass "<0>:Ref Bool"),
                  ("ref \"foo\"", pass "<0>:Ref String"),
                  ("ref unit", pass "<0>:Ref Unit"),
                  ("ref 1.1", pass "<0>:Ref Float"),
                  ("ref 1.1000001", pass "<0>:Ref Float"),
                  ("ref unit; ref true; ref false; ref 1.1000001", pass "<3>:Ref Float"),
                  ("ref ref ref ref unit", pass "<3>:Ref Ref Ref Ref Unit")
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
                    ("lambda x:Top. x", pass "(lambda x.x):(Top -> Top)"),
                    ("lambda x:Bot. x", pass "(lambda x.x):(Bot -> Bot)"),
                    ("(lambda x:Bot.error)", pass "(lambda x.error):(Bot -> Bot)"),
                    ("(lambda x:Bot. x x)", pass "(lambda x.x x):(Bot -> Bot)")
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
                  ),
                  (
                    "if false then {x=true,y=false,a=false} else {y=false,x={},b=false}",
                    pass "{b=false, x={}, y=false}:{x=Top, y=Bool}"
                  )
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
                  ),
                  (
                    "(lambda x:Source Nat.succ (!x)) ref zero",
                    pass "succ zero:Nat"
                  ),
                  (
                    "let sink = ref succ succ succ succ succ succ succ zero in \
                    \let clean = (lambda x:Sink Nat.x := zero) in \
                    \let app = clean sink in \
                    \!sink",
                    pass "zero:Nat"
                  ),
                  (
                    "error true",
                    failed "\"<stdin>\" (line 1, column 7): error"
                  ),
                  (
                    "(lambda x:Bool. x) error",
                    failed "\"<stdin>\" (line 1, column 25): error"
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

        describe "simple error handling" $ do
           tests evalString [
                (
                  "try true with false",
                  pass "true:Bool"
                ),
                (
                  "try error with false",
                  pass "false:Bool"
                ),
                (
                  "try (lambda x:Nat.if zero? x then succ x else error) 1 with false",
                  pass "false:Top"
                ),
                (
                  "try (lambda x:Nat.if zero? x then succ x else error) 0 with false",
                  pass "succ zero:Top"
                ),
                (
                  "try (lambda x:Nat.if zero? x then succ x else error) 1 with 2",
                  pass "succ succ zero:Nat"
                ),
                (
                  "try (lambda x:Nat.if zero? x then succ x else error) 0 with 2",
                  pass "succ zero:Nat"
                ),
                (
                  "(lambda x:Nat.if zero? x then succ x else error) 1",
                  failed "\"<stdin>\" (line 1, column 48): error"
                )
              ]

        describe "assignment and let" $ do
            tests evalString [
                  ("(ref true) := false", pass "unit:Unit"),
                  ("(lambda x:Ref Bool. x := (if (!x) then false else true)) (ref false)", pass "unit:Unit"),
                  ("let x = ref zero in !x", pass "zero:Nat"),
                  ("let x = ref zero in let y = x := (succ !x) in x", pass "<0>:Ref Nat"),
                  ("let x = ref zero in let y = x := (succ !x) in !x", pass "succ zero:Nat"),
                  ("let x = ref zero in let y = x := (succ !x) in y", pass "unit:Unit")
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
                      "(lambda X.lambda x:X.x)",
                      pass "(lambda X.(lambda x.x)):(All X<:Top. (X -> X))"
                    ),
                    (
                      "(lambda X.lambda x:X. x) [All X.X->T]",
                      pass "(lambda x.x):((All X<:Top. (X -> T)) -> (All X<:Top. (X -> T)))"
                    ),
                    (
                      "(lambda X.(lambda x:X. x)) [Bool]",
                      pass "(lambda x.x):(Bool -> Bool)"
                    )
                 ]

        describe "fullref oop" $ do
            describe "Counter" $ do
                let definitions = "\
                \ let newCounter = (lambda c:Unit. \
                \ let state = ref zero in \
                \ let object = {get=(lambda u:Unit. !state), inc=(lambda u:Unit. state := (succ (!state)))} in \
                \ object) in "

                tests evalString [
                      (
                        definitions ++ "newCounter unit",
                        pass "{get=(lambda u.!<0>), inc=(lambda u.<0> := succ !<0>)}:{get=(Unit -> Nat), inc=(Unit -> Unit)}"
                      ),
                      (
                        definitions ++ "\
                        \ let counter = newCounter unit in \
                        \ let x1 = counter.inc unit in \
                        \ let x2 = counter.inc unit in \
                        \ let x3 = counter.inc unit in \
                        \ counter.get unit",
                        pass "succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let counter = newCounter unit in \
                        \ let x1 = counter.inc unit in \
                        \ let x2 = counter.inc unit in \
                        \ counter.get unit",
                        pass "succ succ zero:Nat"
                      )
                 ]

            describe "ResetCounter" $ do
                let definitions = "\
                \ let newResetCounter = (lambda c:Unit.let state = ref zero in \
                \ let object = {\
                \ get=(lambda u:Unit. !state), \
                \ inc=(lambda u:Unit. state := (succ (!state))), \
                \ reset=(lambda u:Unit. state := zero)} \
                \ in object) in "

                tests evalString [
                  (
                    "let state = ref zero in \
                    \let counter = {get=(lambda u:Unit. !state), inc=(lambda u:Unit. state := (succ (!state)))} in \
                    \let t = counter.inc unit in \
                    \counter.get unit",
                    pass "succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newResetCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.reset unit in \
                    \ counter.get unit",
                    pass "zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newResetCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ counter.get unit",
                    pass "succ succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newResetCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.reset unit in \
                    \ counter.get unit",
                    pass "zero:Nat"
                  )
                 ]

            describe "ResetCounter -> BackupCounter" $ do
                let definitions = "\
                \ let resetCounterClass = (lambda state:{x:Ref Nat}.{\
                \   get=(lambda u:Unit. !state.x), \
                \   inc=(lambda u:Unit. state.x := (succ (!(state.x)))), \
                \   reset=(lambda u:Unit. state.x := zero) \
                \ }) in \
                \ let backupCounterClass = (lambda r:{x:Ref Nat, b:Ref Nat}. \
                \ let super = resetCounterClass r in {\
                \   get=super.get,\
                \   inc=super.inc,\
                \   reset=(lambda u:Unit. r.x := !(r.b)),\
                \   backup=(lambda u:Unit. r.b := !(r.x)) \
                \ }) in \
                \ let newBackupCounterClass = (lambda u:Unit.\
                \ let backupCounterRep = {x=ref zero, b=ref zero} in backupCounterClass backupCounterRep \
                \) in "

                tests evalString [
                  (
                    definitions ++ "\
                    \ let counter = newBackupCounterClass unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.backup unit in \
                    \ let x6 = counter.reset unit in \
                    \ counter.get unit",
                    pass "succ succ succ succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newBackupCounterClass unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.backup unit in \
                    \ let x6 = counter.reset unit in \
                    \ let x5 = counter.inc unit in \
                    \ counter.get unit",
                    pass "succ succ succ succ succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newBackupCounterClass unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.backup unit in \
                    \ let x6 = counter.reset unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.reset unit in \
                    \ counter.get unit",
                    pass "succ succ succ succ succ zero:Nat"
                  )
                 ]

            describe "ResetCounter -> BackupCounter -> DoubleBackupCounter" $ do
                let definitions = "\
                    \let resetCounterClass = (lambda state:{x:Ref Nat}. \
                    \ {get=(lambda u:Unit. !state.x), \
                    \  inc=(lambda u:Unit. state.x := (succ (!(state.x)))), \
                    \  reset=(lambda u:Unit. state.x := zero) }) in \
                    \let backupCounterClass = (lambda r:{x:Ref Nat, b:Ref Nat}. \
                    \  let super = resetCounterClass r in \
                    \  {get=super.get,\
                    \   inc=super.inc,\
                    \   reset=(lambda u:Unit. r.x := !(r.b)),\
                    \   backup=(lambda u:Unit. r.b := !(r.x))} \
                    \) in \
                    \let doubleBackupCounterClass = (lambda r:{x:Ref Nat, b:Ref Nat, b2:Ref Nat}. \
                    \  let super = backupCounterClass r in \
                    \  {get=super.get,\
                    \   inc=super.inc,\
                    \   reset=super.reset,\
                    \   backup=super.backup, \
                    \   reset2=(lambda u:Unit. r.x := !(r.b2)),\
                    \   backup2=(lambda u:Unit. r.b2 := !(r.x))} \
                    \) in \
                    \let newDoubleBackupCounterClass = (lambda u:Unit.\
                    \ let doubleBackupCounterRep = {x=ref zero, b=ref zero, b2=ref zero} in \
                    \ doubleBackupCounterClass doubleBackupCounterRep \
                    \) in "

                tests evalString [
                  (
                    definitions ++ "\
                    \ let counter = newDoubleBackupCounterClass unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.backup unit in \
                    \ let x6 = counter.reset unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.reset unit in \
                    \ counter.get unit",
                    pass "succ succ succ succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newDoubleBackupCounterClass unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.backup unit in \
                    \ let x6 = counter.backup2 unit in \
                    \ let x6 = counter.reset unit in \
                    \ let x6 = counter.reset2 unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.reset unit in \
                    \ let x6 = counter.reset2 unit in \
                    \ counter.get unit",
                    pass "succ succ succ succ succ zero:Nat"
                  )
                 ]

            describe "SetCounter" $ do
                let definitions = "\
                \ let setCounterClass = (lambda state:{x:Ref Nat}. \
                \ {get=(lambda u:Unit. !state.x), \
                \  set=(lambda v:Nat. state.x := v), \
                \  reset=(lambda u:Unit. state.x := zero) } \
                \) in \
                \let newSetCounter = (lambda u:Unit.\
                \ let setCounterRep = {x=ref zero} in \
                \ setCounterClass setCounterRep \
                \) in "

                tests evalString [
                  (
                    definitions ++ "\
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ counter.get unit",
                    pass "succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ counter.get unit",
                    pass "succ succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ counter.get unit",
                    pass "succ succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ counter.get unit",
                    pass "succ succ succ zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ let x3 = counter.reset unit in \
                    \ counter.get unit",
                    pass "zero:Nat"
                  ),
                  (
                    definitions ++ "\
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ let x3 = counter.reset unit in \
                    \ let x4 = counter.set (succ succ zero) in \
                    \ counter.get unit",
                    pass "succ succ zero:Nat"
                  )
                 ]

        --  https://github.com/enaudon/TAPL/blob/master/source/fullfsubref/test.f#L66
        describe "Alternative oop" $ do
            describe "CounterRep" $ do
                let definitions = "\
                \ Counterrep = {x: Ref Nat}; \
                \ Setcounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}; \
                \ let setCounterClass = (lambda r:Counterrep.(lambda self: Unit->Setcounter.(lambda p:Unit. \
                \ { \
                \   get=(lambda a:Unit.!(r.x)), \
                \   set=(lambda i:Nat.r.x:=i), \
                \   inc=(lambda z:Unit.let state = self unit in \
                \                      let current = state.get unit in \
                \                      let next = succ current in \
                \                      let result = state.set next in result) \
                \ } \
                \ ))) in \
                \ let newSetCounter = (lambda u:Unit. let r = {x=ref (succ zero)} in (fix (setCounterClass r)) unit) in \
                \ let c = newSetCounter unit in "

                tests evalString [
                      (
                        definitions ++ "c.get unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.get unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.set zero in c.get unit",
                        pass "zero:Nat"
                      )
                 ]

            describe "InstrCounter" $ do
                let definitions = "\
                \ Counterrep = {x: Ref Nat}; \
                \ Setcounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}; \
                \ Instrcounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat}; \
                \ Instrcounterrep = {x: Ref Nat, a: Ref Nat}; \
                \ let setCounterClass = (lambda r:Counterrep.(lambda self: Unit->Setcounter.(lambda p:Unit. \
                \ { \
                \   get=(lambda a:Unit.!(r.x)), \
                \   set=(lambda i:Nat.r.x:=i), \
                \   inc=(lambda z:Unit.let state = self unit in \
                \                      let current = state.get unit in \
                \                      let next = succ current in \
                \                      let result = state.set next in result) \
                \ } \
                \ ))) in \
                \ let newSetCounter = (lambda u:Unit. let r = {x=ref (succ zero)} in fix (setCounterClass r) unit) in \
                \ let instrCounterClass = (lambda r:Instrcounterrep.(lambda self: Unit->Instrcounter.(lambda u:Unit. \
                \    let super = setCounterClass r self unit in \
                \    { \
                \      get = super.get, \
                \      set = (lambda i:Nat.let v = succ !r.a in \
                \                          let vv = r.a := v in \
                \                          super.set i), \
                \      inc = super.inc, \
                \      accesses = (lambda u:Unit. !(r.a)) \
                \    } \
                \ ))) in \
                \ let newInstrCounter = (lambda u:Unit. \
                \    let r = { x=ref (succ zero), a=ref zero } in \
                \    fix (instrCounterClass r) unit \
                \ ) in \
                \ let c = newInstrCounter unit in "

                tests evalString [
                      (
                        definitions ++ "c.get unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.get unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.set zero in c.get unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.accesses unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in let ccc = c.inc unit in c.accesses unit",
                        pass "succ succ zero:Nat"
                      )
                 ]

            describe "ResetInstrCounter" $ do
                let definitions = "\
                \ CounterRep = {x: Ref Nat}; \
                \ SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}; \
                \ InstrCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat}; \
                \ InstrCounterRep = {x: Ref Nat, a: Ref Nat}; \
                \ ResetInstrCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat, reset:Unit->Unit}; \
                \ BackupInstrCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat, backup:Unit->Unit, reset:Unit->Unit}; \
                \ BackupInstrCounterRep = {x: Ref Nat, a: Ref Nat, b: Ref Nat}; \
                \ let setCounterClass = (lambda r:CounterRep.(lambda self:Unit->SetCounter.(lambda p:Unit. \
                \ { \
                \   get=(lambda a:Unit.!(r.x)), \
                \   set=(lambda i:Nat.r.x:=i), \
                \   inc=(lambda z:Unit.let state = self unit in \
                \                      let current = state.get unit in \
                \                      let next = succ current in \
                \                      let result = state.set next in result) \
                \ } \
                \ ))) in \
                \ let newSetCounter = (lambda u:Unit. let r = {x=ref (succ zero)} in fix (setCounterClass r) unit) in \
                \ let instrCounterClass = (lambda r:InstrCounterRep.(lambda self: Unit->InstrCounter.(lambda u:Unit. \
                \    let super = setCounterClass r self unit in \
                \    { \
                \      get = super.get, \
                \      set = (lambda i:Nat.let v = succ !r.a in \
                \                          let vv = r.a := v in \
                \                          super.set i), \
                \      inc = super.inc, \
                \      accesses = (lambda u:Unit. !(r.a)) \
                \    } \
                \ ))) in \
                \ let newInstrCounter = (lambda u:Unit. \
                \    let r = { x=ref (succ zero), a=ref zero } in \
                \    fix (instrCounterClass r) unit \
                \ ) in \
                \ let instrCounterClass = (lambda r:InstrCounterRep.(lambda self:Unit->InstrCounter.(lambda u:Unit. \
                \     let super = setCounterClass r self unit in \
                \     { \
                \         get = (lambda u:Unit.let v = !r.a in let vv = r.a := succ v in super.get unit), \
                \         set = (lambda i:Nat.let v = !r.a in let vv = r.a := succ v in super.set i), \
                \         inc = super.inc, \
                \         accesses = (lambda u:Unit.!r.a) \
                \     } \
                \ ))) in \
                \ let resetInstrCounterClass = (lambda r:InstrCounterRep.(lambda self:Unit->ResetInstrCounter.(lambda u:Unit. \
                \     let super = instrCounterClass r self unit in \
                \     { \
                \         get = super.get, \
                \         set = super.set, \
                \         inc = super.inc, \
                \         accesses = super.accesses, \
                \         reset = (lambda u:Unit. r.x := zero) \
                \     } \
                \ ))) in \
                \ let backupInstrCounterClass = (lambda r:BackupInstrCounterRep.(lambda self:Unit->BackupInstrCounter.(lambda u:Unit. \
                \     let super = resetInstrCounterClass r self unit in \
                \     { \
                \       get = super.get, \
                \       set = super.set, \
                \       inc = super.inc, \
                \       accesses = super.accesses, \
                \       reset = (lambda u:Unit. let v = !r.b in r.x := v), \
                \       backup = (lambda u:Unit. let v = !r.x in r.b := v) \
                \     } \
                \ ))) in \
                \ let newBackupInstrCounter = (lambda u:Unit. \
                \    let r = {x=ref succ zero, a=ref zero, b=ref zero} in \
                \    fix (backupInstrCounterClass r) unit \
                \ ) in \
                \ let c = newBackupInstrCounter unit in "

                tests evalString [
                      (
                        definitions ++ "c.get unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.get unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.set zero in c.get unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.accesses unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let cc = c.inc unit in \
                        \ let ccc = c.inc unit in \
                        \ c.accesses unit",
                        pass "succ succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ c.get unit",
                        pass "succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ let cc = c.backup unit in \
                        \ c.get unit",
                        pass "succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ let cc = c.backup unit in \
                        \ let cd = c.reset unit in \
                        \ c.get unit",
                        pass "succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ let cc = c.backup unit in \
                        \ let ce = c.reset unit in \
                        \ let cd = c.inc unit in \
                        \ c.get unit",
                        pass "succ succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ let cc = c.backup unit in \
                        \ let cd = c.inc unit in \
                        \ let ce = c.reset unit in \
                        \ c.get unit",
                        pass "succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ let cc = c.backup unit in \
                        \ let cd = c.inc unit in \
                        \ let ce = c.reset unit in \
                        \ let cc = c.backup unit in \
                        \ let ce = c.reset unit in \
                        \ c.get unit",
                        pass "succ succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ let cc = c.backup unit in \
                        \ let cd = c.inc unit in \
                        \ let ce = c.reset unit in \
                        \ let cc = c.backup unit in \
                        \ let ce = c.reset unit in \
                        \ c.accesses unit",
                        pass "succ succ succ succ succ succ zero:Nat"
                      )
                 ]

            --https://github.com/enaudon/TAPL/blob/master/source/fullfsubref/test.f#L191
            describe "MethodTables" $ do
                let definitions = "\
                \CounterRep = {x: Ref Nat}; \
                \SetCounterMethodTable = { \
                \   get: Ref <none:Unit, some:Unit->Nat>, \
                \   set: Ref <none:Unit, some:Nat->Unit>, \
                \   inc: Ref <none:Unit, some:Unit->Unit> \
                \ }; \
                \let packGet = (lambda f:Unit->Nat.<some=f> as <none:Unit, some:Unit->Nat>) in \
                \let unpackGet = (lambda mt:SetCounterMethodTable.case !(mt.get) of <none=x> -> error | <some=f> -> f) in \
                \let packSet = (lambda f:Nat->Unit.<some=f> as <none:Unit, some:Nat->Unit>) in \
                \let unpackSet = (lambda mt:SetCounterMethodTable.case !(mt.set) of <none=x> -> error | <some=f> -> f) in \
                \let packInc = (lambda f:Unit->Unit.<some=f> as <none:Unit, some:Unit->Unit>) in \
                \let unpackInc = (lambda mt:SetCounterMethodTable.case !(mt.inc) of <none=x> -> error | <some=f> -> f) in \
                \let setCounterClass = (lambda r:CounterRep.lambda self:SetCounterMethodTable. \
                \   let get = packGet (lambda u:Unit. !(r.x)) in \
                \   let set = packSet (lambda i:Nat. r.x:=i) in \
                \   let inc = packInc (lambda u:Unit. unpackSet self (succ (unpackGet self unit))) in \
                \   let v = self.get := get in \
                \   let vv = self.set := set in \
                \   let vvv = self.inc := inc in \
                \   unit \
                \) in \
                \"

                tests evalString [
                      (
                        definitions ++ "unit",
                        pass "unit:Unit"
                      )
                 ]

            --https://github.com/enaudon/TAPL/blob/master/source/fullfsubref/test.f#L309
            describe "Cool SetCounter" $ do
                let definitions = "\
                \ CounterRep = {x: Ref Nat}; \
                \ SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}; \
                \let setCounterClass = (lambda M<:SetCounter.(lambda R<:CounterRep.(lambda self: Ref(R->M).(lambda r: R. \
                \    { \
                \      get = (lambda u:Unit.!(r.x)), \
                \      set = (lambda i:Nat.r.x := i), \
                \      inc = (lambda z:Unit.let s = !self in \
                \                           let state = s r in \
                \                           let current = state.get unit in \
                \                           let next = succ current in \
                \                           let result = state.set next in result) \
                \    } \
                \)))) in \
                \let newSetCounter = (lambda u:Unit. \
                \    let m = ref (lambda r:CounterRep.error as SetCounter) in \
                \    let sa = setCounterClass [SetCounter] in \
                \    let sb = sa [CounterRep] in \
                \    let sm = sb m in \
                \    let v = m := sm in \
                \    let r = { x=ref succ zero } in \
                \    sm r \
                \) in \
                \ let c = newSetCounter unit in \
                \"

                tests evalString [
                      (
                        definitions ++ "c.get unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.get unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.set zero in c.get unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ c.get unit",
                        pass "succ succ succ zero:Nat"
                      )
                 ]

            describe "Cool InstCounter" $ do
                let definitions = "\
                \ CounterRep = {x: Ref Nat}; \
                \ SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}; \
                \ InstrCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat}; \
                \ InstrCounterRep = {x: Ref Nat, a: Ref Nat}; \
                \let setCounterClass = (lambda M<:SetCounter.(lambda R<:CounterRep.(lambda self: Ref(R->M).(lambda r: R. \
                \    { \
                \      get = (lambda u:Unit.!(r.x)), \
                \      set = (lambda i:Nat.r.x := i), \
                \      inc = (lambda z:Unit.let s = !self in \
                \                           let state = s r in \
                \                           let current = state.get unit in \
                \                           let next = succ current in \
                \                           let result = state.set next in result) \
                \    } \
                \)))) in \
                \let newSetCounter = (lambda u:Unit. \
                \    let m = ref (lambda r:CounterRep.error as SetCounter) in \
                \    let sa = setCounterClass [SetCounter] in \
                \    let sb = sa [CounterRep] in \
                \    let sm = sb m in \
                \    let v = m := sm in \
                \    let r = { x=ref succ zero } in \
                \    sm r \
                \) in \
                \let instrCounterClass = (lambda M<:InstrCounter.(lambda R<:InstrCounterRep.(lambda self: Ref(R->M).(lambda r: R. \
                \    let m = setCounterClass [M] in \
                \    let mr = m [R] in \
                \    let super = mr self in \
                \    let parent = super r in \
                \    { \
                \        get = parent.get, \
                \        inc = parent.inc, \
                \        set = (lambda i:Nat.let current = !(r.a) in \
                \                            let next = succ current in \
                \                            let v = r.a := next in \
                \                            parent.set i), \
                \        accesses = (lambda u:Unit.!(r.a)) \
                \    } \
                \)))) in \
                \let newInstrCounter = (lambda u:Unit. \
                \    let m = ref (lambda r:InstrCounterRep. error as InstrCounter) in \
                \    let ma = instrCounterClass [InstrCounter] in \
                \    let mb = ma [InstrCounterRep] in \
                \    let mc = mb m in \
                \    let v = m := mc in \
                \    let r = { x=ref succ zero, a=ref zero } in \
                \    mc r \
                \) in \
                \let c = newInstrCounter unit in \
                \"

                tests evalString [
                      (
                        definitions ++ "c.get unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "c.accesses unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.get unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.set zero in c.get unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.accesses unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in let ccc = c.inc unit in c.accesses unit",
                        pass "succ succ zero:Nat"
                      )
                 ]

            --https://github.com/enaudon/TAPL/blob/master/source/fullfsubref/test.f#L368
            describe "James Reily's alternative: SetCounter" $ do
                let definitions = "\
                \ CounterRep = {x: Ref Nat}; \
                \ SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}; \
                \let dummySetCounter = { \
                \    get=(lambda x:Unit.zero), \
                \    set=(lambda x:Nat.unit), \
                \    inc=(lambda x:Unit.unit) \
                \} in \
                \let setCounterClass = (lambda r:CounterRep.(lambda self:Source SetCounter. \
                \    { \
                \        get = (lambda u:Unit.!(r.x)), \
                \        set = (lambda i:Nat. r.x:=i), \
                \        inc = (lambda u:Unit. let s = !self in \
                \                              let current = s.get unit in \
                \                              let next = succ current in \
                \                              s.set next) \
                \    } \
                \)) in \
                \let newSetCounter = (lambda u:Unit. \
                \    let r = {x=ref succ zero} in \
                \    let caux = ref dummySetCounter in \
                \    let v = setCounterClass r caux in \
                \    let vv = caux := v in \
                \    !caux \
                \) in \
                \ let c = newSetCounter unit in \
                \"

                tests evalString [
                      (
                        definitions ++ "c.get unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.get unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.set zero in c.get unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "\
                        \ let ca = c.inc unit in \
                        \ let cb = c.inc unit in \
                        \ c.get unit",
                        pass "succ succ succ zero:Nat"
                      )
                 ]

            describe "James Reily's alternative: InstrCounter" $ do
                let definitions = "\
                \ Counter = {get:Unit->Nat, inc:Unit->Unit}; \
                \ CounterRep = {x: Ref Nat}; \
                \ SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit}; \
                \ InstrCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit, accesses:Unit->Nat}; \
                \ InstrCounterRep = {x: Ref Nat, a: Ref Nat}; \
                \let dummySetCounter = { \
                \    get=(lambda x:Unit.zero), \
                \    set=(lambda x:Nat.unit), \
                \    inc=(lambda x:Unit.unit) \
                \} in \
                \let dummyInstrCounter = { \
                \    get=(lambda x:Unit.zero), \
                \    set=(lambda x:Nat.unit), \
                \    inc=(lambda x:Unit.unit), \
                \    accesses=(lambda x:Unit.zero) \
                \} in \
                \let setCounterClass = (lambda r:CounterRep.(lambda self:Source SetCounter. \
                \    { \
                \        get = (lambda u:Unit.!(r.x)), \
                \        set = (lambda i:Nat. r.x:=i), \
                \        inc = (lambda u:Unit. let s = !self in \
                \                              let current = s.get unit in \
                \                              let next = succ current in \
                \                              s.set next) \
                \    } \
                \)) in \
                \let newSetCounter = (lambda u:Unit. \
                \    let r = {x=ref succ zero} in \
                \    let caux = ref dummySetCounter in \
                \    let v = setCounterClass r caux in \
                \    let vv = caux := v in \
                \    !caux \
                \) in \
                \let instrCounterClass = (lambda r:InstrCounterRep.lambda self:Source InstrCounter. \
                \    let super = setCounterClass r self in \
                \        { \
                \            get = super.get, \
                \            set = (lambda i:Nat.let current = !(r.a) in \
                \                                let next = succ current in \
                \                                let v = r.a := next in \
                \                                super.set i), \
                \            inc = super.inc, \
                \            accesses = (lambda u:Unit. !(r.a)) \
                \        } \
                \) in \
                \let newInstrCounter = (lambda i:Unit. \
                \    let r = {x=ref succ zero, a=ref zero} in \
                \    let caux = ref dummyInstrCounter in \
                \    let v = instrCounterClass r caux in \
                \    let vv = caux := v in \
                \    !caux \
                \) in \
                \ let c = newInstrCounter unit in \
                \"

                tests evalString [
                      (
                        definitions ++ "c.get unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "c.accesses unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.get unit",
                        pass "succ succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.set zero in c.get unit",
                        pass "zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in c.accesses unit",
                        pass "succ zero:Nat"
                      ),
                      (
                        definitions ++ "let cc = c.inc unit in let ccc = c.inc unit in c.accesses unit",
                        pass "succ succ zero:Nat"
                      )
                 ]