{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullRefSpec where

import Test.Hspec
import Language.TAPL.FullRef.Evaluator (eval)

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

        describe "pairs" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("{true, false}", "{true,false}:{Bool*Bool}"),
                  ("{true, unit}", "{true,unit}:{Bool*Unit}"),
                  ("{1.1, \"foo\"}", "{1.1,\"foo\"}:{Float*String}"),
                  ("{(lambda x:Unit.x), (lambda x:Unit.x)}", "{(lambda x.x),(lambda x.x)}:{Unit -> Unit*Unit -> Unit}"),
                  ("{ref (lambda x:Unit.x), ref ref ref unit}", "{<0>,<3>}:{Ref Unit -> Unit*Ref Ref Ref Unit}"),
                  ("{true, false}.0", "true:Bool"),
                  ("{true, unit}.1", "unit:Unit"),
                  ("{1.1, \"foo\"}.0", "1.1:Float"),
                  ("{1.1, \"foo\"}.1", "\"foo\":String"),
                  ("{(lambda x:Unit.x), (lambda x:Unit.x)}.1", "(lambda x.x):Unit -> Unit"),
                  ("{ref (lambda x:Unit.x), ref ref ref unit}.0", "<0>:Ref Unit -> Unit")
                ]
           mapM_ test examples

        describe "records" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("{a=true, b=false}", "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("{a=true, b=unit}", "{a=true, b=unit}:{a=Bool, b=Unit}"),
                  ("{a=1.1, b=\"foo\"}", "{a=1.1, b=\"foo\"}:{a=Float, b=String}"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}", "{c=(lambda x.x), d=(lambda x.x)}:{c=Unit -> Unit, d=Unit -> Unit}"),
                  ("{key1=ref (lambda x:Unit.x), key2=ref ref ref unit}", "{key1=<0>, key2=<3>}:{key1=Ref Unit -> Unit, key2=Ref Ref Ref Unit}"),
                  ("{a=true, b=false}.a", "true:Bool"),
                  ("{a=true, b=unit}.b", "unit:Unit"),
                  ("{a=1.1, b=\"foo\"}.b", "\"foo\":String"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d", "(lambda x.x):Unit -> Unit"),
                  ("{key1=ref (lambda x:Unit.x), key2=ref ref ref unit}.key2", "<3>:Ref Ref Ref Unit"),
                  ("!{key1=ref (lambda x:Unit.x), key2=ref ref ref unit}.key2", "<2>:Ref Ref Unit")
                ]
           mapM_ test examples

        describe "variants" $ do
          let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
              examples = [
                ("<a=unit> as <a:Unit,b:Unit,c:Nat>", "<a=unit>:<a:Unit, b:Unit, c:Nat>"),
                ("(lambda x:<a:Unit,b:Unit,c:Nat>.x)", "(lambda x.x):<a:Unit, b:Unit, c:Nat> -> <a:Unit, b:Unit, c:Nat>"),
                ("let z = <a=true> as <a:Bool,b:Unit,c:Nat> in (lambda x:<a:Bool, b:Unit, c:Nat>.x) z", "<a=true>:<a:Bool, b:Unit, c:Nat>"),
                ("case <b=zero> as <a:Int,b:Nat> of <b=x> -> succ succ pred pred pred x | <a=y> -> zero", "succ succ zero:Nat"),
                ("case <b=zero> as <a:Nat,b:Nat> of <b=x> -> if (zero? x) then false else true | <a=y> -> true", "false:Bool"),
                ("case <b=succ zero> as <a:Nat,b:Nat> of <a=x> -> <a=succ x> as <a:Nat>| <b=y> -> <a=succ y> as <a:Nat>", "<a=succ succ zero>:<a:Nat>")
               ]
          mapM_ test examples

        describe "ascribe" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("\"foo\" as String", "\"foo\":String"),
                  ("false as Bool", "false:Bool"),
                  ("1.1 as Float", "1.1:Float"),
                  ("true as Bool", "true:Bool"),
                  ("unit as Unit", "unit:Unit"),
                  ("1.1000001 as Float", "1.1000001:Float")
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
                    ("(lambda x:Float.x)", "(lambda x.x):Float -> Float"),
                    ("(lambda x:Bool.lambda y:String.x)", "(lambda x.(lambda y.x)):Bool -> String -> Bool"),
                    ("(lambda x:Bool.lambda y:A.x)", "(lambda x.(lambda y.x)):Bool -> A -> Bool"),
                    ("(lambda x:Bool.lambda y:A.y)", "(lambda x.(lambda y.y)):Bool -> A -> A"),
                    ("(lambda x:(Bool -> A).lambda y:A.y)", "(lambda x.(lambda y.y)):Bool -> A -> A -> A"),
                    ("(lambda x:A -> A.lambda y:A.x y)", "(lambda x.(lambda y.x y)):A -> A -> A -> A"),
                    ("(lambda x:A.x)", "(lambda x.x):A -> A"),
                    ("(lambda x:Top.x)", "(lambda x.x):Top -> Top"),
                    ("(lambda x:Bot.x)", "(lambda x.x):Bot -> Bot"),
                    ("(lambda x:Bot.x)", "(lambda x.x):Bot -> Bot"),
                    ("(lambda x:{Bot*Top}.x.1)", "(lambda x.x.1):{Bot*Top} -> Top"),
                    ("(lambda x:{Bool*{Unit*Top}}.true)", "(lambda x.true):{Bool*{Unit*Top}} -> Bool"),
                    ("(lambda x:{Bool*{Unit*(A->B)}}.x)", "(lambda x.x):{Bool*{Unit*A -> B}} -> {Bool*{Unit*A -> B}}"),
                    ("(lambda x:{Bool*{Unit*{Unit*Float}}}.x)", "(lambda x.x):{Bool*{Unit*{Unit*Float}}} -> {Bool*{Unit*{Unit*Float}}}")
                 ]
            mapM_ test examples

    describe "operations" $ do
        describe "condition" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("if true then unit else unit", "unit:Unit"),
                  ("if false then \"foo\" else \"bar\"", "\"bar\":String"),
                  ("if true then 3.14 else 9.8", "3.14:Float")
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
                  ("(lambda x:Bool -> Bool.lambda y:A. x true)", "(lambda x.(lambda y.x true)):Bool -> Bool -> A -> Bool"),
                  ("(lambda x:Nat. succ x) succ zero", "succ succ zero:Nat"),
                  ("(lambda x:Bot.x x)", "(lambda x.x x):Bot -> Bot"),
                  ("(lambda x:Top.x)(lambda x:Top.x)", "(lambda x.x):Top -> Top"),
                  ("(lambda x:(Top -> Top). x) (lambda x:Top. x)", "(lambda x.x):Top -> Top"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) pred zero", "(lambda y.(lambda z.if zero? zero then y else z)):Float -> Float -> Float"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) zero 3.14 9.8", "3.14:Float"),
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
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14 9.8", "9.8:Float"),
                  ("(lambda x:{a:Bool}.x) {a=true, b=false}", "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("(lambda x:{a:Bool}.if x.a then false else true) {a=true, b=false}", "false:Bool")
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

        describe "fix" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("let diverge = (lambda u:Unit.fix (lambda x:T.x)) in diverge", "(lambda u.(lambda x.x)):Unit -> T"),
                  (
                    "let ff = (lambda ie:Nat -> Bool.lambda x:Nat.if zero? x then true else (if zero? (pred x) then false else ie (pred pred x))) in let iseven = fix ff in iseven",
                    "(lambda x.if zero? x then true else if zero? pred x then false else (lambda ie.(lambda x'.if zero? x' then true else if zero? pred x' then false else ie pred pred x')) pred pred x):Nat -> Bool"
                  )
                 ]
            mapM_ test examples

        describe "oop" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  (
                    "let state = ref zero in \
                    \let counter = {get=(lambda u:Unit. !state), inc=(lambda u:Unit. state := (succ (!state)))} in \
                    \let t = counter.inc unit in \
                    \counter.get unit",
                    "succ zero:Nat"
                  ),
                  (
                    "let newCounter = (lambda c:Unit.let state = ref zero in let object = {get=(lambda u:Unit. !state), inc=(lambda u:Unit. state := (succ (!state)))} in object) in \
                    \ newCounter unit",
                    "{get=(lambda u.!<0>), inc=(lambda u.<0> := succ !<0>)}:{get=Unit -> Nat, inc=Unit -> Unit}"
                  ),
                  (
                    "let newCounter = (lambda c:Unit.let state = ref zero in let object = {get=(lambda u:Unit. !state), inc=(lambda u:Unit. state := (succ (!state)))} in object) in \
                    \ let counter = newCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ counter.get unit",
                    "succ succ succ zero:Nat"
                  ),
                  (
                    "let newCounter = (lambda c:Unit.let state = ref zero in let object = {get=(lambda u:Unit. !state), inc=(lambda u:Unit. state := (succ (!state)))} in object) in \
                    \ let counter = newCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ counter.get unit",
                    "succ succ zero:Nat"
                  ),
                  (
                    "let newResetCounter = (lambda c:Unit.let state = ref zero in \
                    \let object = {\
                    \get=(lambda u:Unit. !state), \
                    \inc=(lambda u:Unit. state := (succ (!state))), \
                    \reset=(lambda u:Unit. state := zero)} \
                    \in object) in \
                    \ let counter = newResetCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.reset unit in \
                    \ counter.get unit",
                    "zero:Nat"
                  ),
                  (
                    "let newResetCounter = (lambda c:Unit.let state = {x=ref zero} in \
                    \let object = {\
                    \get=(lambda u:Unit. !state.x), \
                    \inc=(lambda u:Unit. state.x := (succ (!(state.x)))), \
                    \reset=(lambda u:Unit. state.x := zero)} \
                    \in object) in \
                    \ let counter = newResetCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ counter.get unit",
                    "succ succ succ zero:Nat"
                  ),
                  (
                    "let newResetCounter = (lambda c:Unit.let state = {x=ref zero} in \
                    \let object = {\
                    \get=(lambda u:Unit. !state.x), \
                    \inc=(lambda u:Unit. state.x := (succ (!(state.x)))), \
                    \reset=(lambda u:Unit. state.x := zero)} \
                    \in object) in \
                    \ let counter = newResetCounter unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.reset unit in \
                    \ counter.get unit",
                    "zero:Nat"
                  ),
                  (
                    "let resetCounterClass = (lambda state:{x:Ref Nat}. \
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
                    \let newBackupCounterClass = (lambda u:Unit.\
                    \ let backupCounterRep = {x=ref zero, b=ref zero} in \
                    \ backupCounterClass backupCounterRep \
                    \) in \
                    \ let counter = newBackupCounterClass unit in \
                    \ let x1 = counter.inc unit in \
                    \ let x2 = counter.inc unit in \
                    \ let x3 = counter.inc unit in \
                    \ let x4 = counter.inc unit in \
                    \ let x5 = counter.inc unit in \
                    \ let x6 = counter.backup unit in \
                    \ let x6 = counter.reset unit in \
                    \ counter.get unit",
                    "succ succ succ succ succ zero:Nat"
                  ),
                  (
                    "let newResetCounterClass = (lambda state:{x:Ref Nat}. \
                    \ {get=(lambda u:Unit. !state.x), \
                    \  inc=(lambda u:Unit. state.x := (succ (!(state.x)))), \
                    \  reset=(lambda u:Unit. state.x := zero) }) in \
                    \let backupCounterClass = (lambda r:{x:Ref Nat, b:Ref Nat}. \
                    \  let super = newResetCounterClass r in \
                    \  {get=super.get,\
                    \   inc=super.inc,\
                    \   reset=(lambda u:Unit. r.x := !(r.b)),\
                    \   backup=(lambda u:Unit. r.b := !(r.x))} \
                    \) in \
                    \let newBackupCounterClass = (lambda u:Unit.\
                    \ let backupCounterRep = {x=ref zero, b=ref zero} in \
                    \ backupCounterClass backupCounterRep \
                    \) in \
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
                    "succ succ succ succ succ succ zero:Nat"
                  ),
                  (
                    "let newResetCounterClass = (lambda state:{x:Ref Nat}. \
                    \ {get=(lambda u:Unit. !state.x), \
                    \  inc=(lambda u:Unit. state.x := (succ (!(state.x)))), \
                    \  reset=(lambda u:Unit. state.x := zero) }) in \
                    \let backupCounterClass = (lambda r:{x:Ref Nat, b:Ref Nat}. \
                    \  let super = newResetCounterClass r in \
                    \  {get=super.get,\
                    \   inc=super.inc,\
                    \   reset=(lambda u:Unit. r.x := !(r.b)),\
                    \   backup=(lambda u:Unit. r.b := !(r.x))} \
                    \) in \
                    \let newBackupCounterClass = (lambda u:Unit.\
                    \ let backupCounterRep = {x=ref zero, b=ref zero} in \
                    \ backupCounterClass backupCounterRep \
                    \) in \
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
                    "succ succ succ succ succ zero:Nat"
                  ),
                  (
                    "let resetCounterClass = (lambda state:{x:Ref Nat}. \
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
                    \) in \
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
                    "succ succ succ succ succ zero:Nat"
                  ),
                  (
                    "let resetCounterClass = (lambda state:{x:Ref Nat}. \
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
                    \) in \
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
                    "succ succ succ succ succ zero:Nat"
                  ),
                  (
                    "let setCounterClass = (lambda state:{x:Ref Nat}. \
                    \ {get=(lambda u:Unit. !state.x), \
                    \  set=(lambda v:Nat. state.x := v), \
                    \  reset=(lambda u:Unit. state.x := zero) } \
                    \) in \
                    \let newSetCounter = (lambda u:Unit.\
                    \ let setCounterRep = {x=ref zero} in \
                    \ setCounterClass setCounterRep \
                    \) in \
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ counter.get unit",
                    "succ succ zero:Nat"
                  ),
                  (
                    "let setCounterClass = (lambda state:{x:Ref Nat}. \
                    \ {get=(lambda u:Unit. !state.x), \
                    \  set=(lambda v:Nat. state.x := v), \
                    \  reset=(lambda u:Unit. state.x := zero) } \
                    \) in \
                    \let newSetCounter = (lambda u:Unit.\
                    \ let setCounterRep = {x=ref zero} in \
                    \ setCounterClass setCounterRep \
                    \) in \
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ counter.get unit",
                    "succ succ succ zero:Nat"
                  ),
                  (
                    "let setCounterClass = (lambda state:{x:Ref Nat}. \
                    \ fix (lambda self:{get:Unit->Nat,set:Nat->Unit}. \
                    \       {get=(lambda u:Unit. !state.x), \
                    \        set=(lambda v:Nat. state.x := v), \
                    \        inc=(lambda u:Unit. self.set (succ (self.get unit)))} \
                    \     )\
                    \) in \
                    \let newSetCounter = (lambda u:Unit.\
                    \ let setCounterRep = {x=ref zero} in \
                    \ setCounterClass setCounterRep \
                    \) in \
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ counter.get unit",
                    "succ succ succ zero:Nat"
                  ),
                  (
                    "let setCounterClass = (lambda state:{x:Ref Nat}. \
                    \ (lambda self:{get:Unit->Nat,set:Nat->Unit}. \
                    \       {get=(lambda u:Unit. !state.x), \
                    \        set=(lambda v:Nat. state.x := v), \
                    \        inc=(lambda u:Unit. self.set (succ (self.get unit)))} \
                    \     )\
                    \) in \
                    \let newSetCounter = (lambda u:Unit.\
                    \ let setCounterRep = {x=ref zero} in \
                    \ fix (setCounterClass setCounterRep) \
                    \) in \
                    \ let counter = newSetCounter unit in \
                    \ let x1 = counter.get unit in \
                    \ let x2 = counter.set (succ succ zero) in \
                    \ let x2 = counter.set (succ succ succ zero) in \
                    \ counter.get unit",
                    "succ succ succ zero:Nat"
                  )
                 ]
            mapM_ test examples