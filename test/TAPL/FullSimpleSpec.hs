{-# LANGUAGE FlexibleContexts #-}

module TAPL.FullSimpleSpec where

import Test.Hspec
import TAPL.FullSimple

spec :: Spec
spec = do
  describe "eval" $ do
    it "true" $ do
        eval "true" `shouldBe` "true:Bool"

    it "false" $ do
        eval "false" `shouldBe` "false:Bool"

    it "\"foo\"" $ do
        eval "\"foo\"" `shouldBe` "foo:String"

    it "unit" $ do
        eval "unit" `shouldBe` "unit:Unit"

    it "1.1" $ do
        eval "1.1" `shouldBe` "1.1:Float"

    it "1.1000001" $ do
        eval "1.1000001" `shouldBe` "1.1000001:Float"

    it "{true, unit}" $ do
        eval "{true, unit}" `shouldBe` "{true,unit}:Bool x Unit"

    it "{true, unit}.0" $ do
        eval "{true, unit}.0" `shouldBe` "true:Bool"

    it "{true, unit}.1" $ do
        eval "{true, unit}.1" `shouldBe` "unit:Unit"

    it "{a=true, b=unit,c=1.1, d=\"test\",e=(λx:Unit.{x,x})}" $ do
        let x = eval "{a=true, b=unit,c=1.1, d=\"test\",e=(λx:Unit.{x,x}), f={0,unit}, g={a=succ 0},i=(if isZero 0 then true else false)}"
            y = "{a=true,b=unit,c=1.1,d=test,e=(\955x.{x,x}),f={0,unit},g={a=succ 0},i=true}:{a=Bool,b=Unit,c=Float,d=String,e=(Unit->Unit x Unit),f=Nat x Unit,g={a=Nat},i=Bool}"
        x `shouldBe` y

    it "(λx:{a=Bool}.x) {a=true} -> {a=true}:{a=Bool}" $ do
        eval "(λx:{a=Bool}.x) {a=true}" `shouldBe` "{a=true}:{a=Bool}"

    it "(λx:{a=Bool}.x) {a=unit} -> type misssmatch" $ do
        eval "(λx:{a=Bool}.x) {a=unit}" `shouldBe` "type mismatch in 1:17"

    it "(λx:{a=Bool}.x) {b=true} -> type misssmatch" $ do
        eval "(λx:{a=Bool}.x) {b=true}" `shouldBe` "type mismatch in 1:17"

    it "(λx:{a=Bool}.x) {a=true, b=unit} -> type misssmatch" $ do
        eval "(λx:{a=Bool}.x) {a=true, b=unit}" `shouldBe` "type mismatch in 1:17"

    it "(λx:{a=Bool}.x) {a=true, b=unit} -> type misssmatch" $ do
        eval "(λx:{a=Bool}.if true then x else {a=0}) {a=true}" `shouldBe` "arms of conditional have different types 1:39"

    it "(λx:{a=Bool}.x) {a=true, b=unit} -> type misssmatch" $ do
        eval "(λx:{a=Bool}.if x then x else {a=0}) {a=true}" `shouldBe` "guard of conditional not a boolean 1:36"

    it "(λx:Bool x Unit.x.1) {true, unit} -> unit:Unit" $ do
        eval "(λx:Bool x Unit.x.1) {true, unit}" `shouldBe` "unit:Unit"

    it "(λx:Unit.{x,x}) unit -> {unit,unit}:Unit x Unit" $ do
        eval "(λx:Unit.{x,x}) unit" `shouldBe` "{unit,unit}:Unit x Unit"

    it "(λx:Float.{x,x}) 1.1 -> {1.1,1.1}:Float x Float" $ do
        eval "(λx:Float.{x,x}) 1.1" `shouldBe` "{1.1,1.1}:Float x Float"

    it "(λx:Nat.{if isZero x then true else false, succ x}) 0 -> {true, succ 0}:Nat x Nat" $ do
        eval "(λx:Nat.{if isZero x then true else false, succ x}) 0" `shouldBe` "{true,succ 0}:Bool x Nat"

    it "(λx:{a=Bool}.x.a) {a=true}" $ do
        eval "(λx:{a=Bool}.x.a) {a=true}" `shouldBe` "true:Bool"

    it "(λx:{a=Bool}.x.a) {b=true}" $ do
        eval "(λx:{a=Bool}.x.a) {b=true}" `shouldBe` "type mismatch in 1:19"

    it "(λx:{a=Nat,b=Nat}.if isZero (x.a) then x.b else pred (x.a)) {a=0,b=succ 0}" $ do
        eval "(λx:{a=Nat,b=Nat}.if isZero (x.a) then x.b else pred (x.a)) {a=0,b=succ 0}" `shouldBe` "succ 0:Nat"

    it "{a=0,b={a=0,b={a=0,b={a=0,b=0}}}}" $ do
        eval "{a=0,b={a=0,b={a=0,b={a=0,b=0}}}}" `shouldBe` "{a=0,b={a=0,b={a=0,b={a=0,b=0}}}}:{a=Nat,b={a=Nat,b={a=Nat,b={a=Nat,b=Nat}}}}"

    it "(λx:Unit.true) false -> type missmatch" $ do
        eval "(λx:Unit.true) false" `shouldBe` "type mismatch in 1:16"

    it "(λx:Unit.true) unit -> true" $ do
        eval "(λx:Unit.true) unit" `shouldBe` "true:Bool"

    it "(λx:String.x) false -> type missmatch" $ do
        eval "(λx:String.x) false" `shouldBe` "type mismatch in 1:15"

    it "(λx:String.x) \"test\" -> test" $ do
        eval "(λx:String.x) \"test\"" `shouldBe` "test:String"

    it "(λx:Bool.x) false -> false:Bool" $ do
        eval "(λx:Bool.x) false" `shouldBe` "false:Bool"

    it "(λx:Bool.x) \"test\" -> test" $ do
        eval "(λx:Bool.x) \"test\"" `shouldBe` "type mismatch in 1:13"

    it "(λx:Float.x) \"test\" -> type mismatch" $ do
        eval "(λx:Float.x) \"test\"" `shouldBe` "type mismatch in 1:14"

    it "(λx:Float.x) 1.1112 -> type mismatch" $ do
        eval "(λx:Float.x) 1.1112" `shouldBe` "1.1112:Float"

    it "(λx:Nat.succ succ x) 1.11 -> type missmatch" $ do
        eval "(λx:Nat.succ succ x) 1.11" `shouldBe` "type mismatch in 1:22"

    it "(λx:Nat.succ succ x)" $ do
        eval "(λx:Nat.succ succ x)" `shouldBe` "(λx.succ succ x):(Nat->Nat)"

    it "(λx:Float.λy:Nat.succ succ y) 123.4" $ do
        eval "(λx:Float.λy:Nat.succ succ y) 123.4" `shouldBe` "(λy.succ succ y):(Nat->Nat)"

    it "(λx:Float.4.9) 3.4 -> 4.9:Float" $ do
        eval "(λx:Float.4.9) 3.4" `shouldBe` "4.9:Float"

    it "(λx:Nat.succ succ x) 0 -> succ succ 0:Nat" $ do
        eval "(λx:Nat.succ succ x) 0 " `shouldBe` "succ succ 0:Nat"

    it "(λx:Nat->Nat.x 0)(λx:Nat.succ x) -> succ 0:Nat" $ do
        eval "(λx:Nat->Nat.x 0)(λx:Nat.succ x)" `shouldBe` "succ 0:Nat"

    it "(λx:Nat->String.x 0)(λx:Nat.if isZero x then \"zero\" else \"notzero\") -> zero:String" $ do
        eval "(λx:Nat->String.x 0)(λx:Nat.if isZero x then \"zero\" else \"notzero\")" `shouldBe` "zero:String"

    it "(λx:Nat->String.x (succ 0))(λx:Nat.if isZero x then \"zero\" else \"notzero\") -> notzero:String" $ do
        eval "(λx:Nat->String.x (succ 0))(λx:Nat.if isZero x then \"zero\" else \"notzero\")" `shouldBe` "notzero:String"

    it "if true then true else false -> true:Bool" $ do
        eval "if true then true else false" `shouldBe` "true:Bool"

    it "if true then unit else unit -> unit:Unit" $ do
        eval "if true then unit else unit" `shouldBe` "unit:Unit"

    it "if true then \"hello\" else \"world\" -> hello:String" $ do
        eval "if true then \"hello\" else \"world\"" `shouldBe` "hello:String"

    it "if (λx:Bool.x) false then \"hello\" else \"world\" -> world:String" $ do
        eval "if (λx:Bool.x) false then \"hello\" else \"world\"" `shouldBe` "world:String"

    it "if (λx:Bool.true) false then \"hello\" else \"world\" -> world:String" $ do
        eval "if (λx:Bool.true) false then \"hello\" else \"world\"" `shouldBe` "hello:String"

    it "if \"test\" then \"hello\" else \"world\" -> guard of conditional not a boolean 1:36" $ do
        eval "if \"test\" then \"hello\" else \"world\"" `shouldBe` "guard of conditional not a boolean 1:36"

    it "if unit then \"hello\" else \"world\" -> guard of conditional not a boolean 1:36" $ do
        eval "if unit then \"hello\" else \"world\"" `shouldBe` "guard of conditional not a boolean 1:34"

    it "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true -> (λy.(λz.if true then y else z)):(Bool->Bool)" $ do
        eval "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true" `shouldBe` "(λy.(λz.if true then y else z)):(Bool->(Bool->Bool))"

    it "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true -> (λz.if true then true else z):(Bool->Bool)" $ do
        eval "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true" `shouldBe` "(λz.if true then true else z):(Bool->Bool)"

    it "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true false -> true:Bool" $ do
        eval "(λx:Bool.λy:Bool.λz:Bool.if x then y else z) true true false" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true) -> true:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true)" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true) -> true:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true)" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true) -> true:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then false else true)" `shouldBe` "true:Bool"

    it "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then true else false) -> false:Bool" $ do
        eval "(λx:Bool->Bool.if x false then true else false)(λx:Bool. if x then true else false)" `shouldBe` "false:Bool"

    it "(λx:String->Bool.if (x \"test\") then \"foo\" else \"bar\")(λx:String.false) -> bar:String" $ do
        eval "(λx:String->Bool.if (x \"test\") then \"foo\" else \"bar\")(λx:String.false)" `shouldBe` "bar:String"

    it "(λx:String->Bool.if (x \"test\") then \"foo\" else \"bar\")(λx:String.true) -> foo:String" $ do
        eval "(λx:String->Bool.if (x \"test\") then \"foo\" else \"bar\")(λx:String.true)" `shouldBe` "foo:String"

    it "(λx:String.λy:String.λz:String.z) \"a\" -> (λy.(λz.z)):(String->(String->String))" $ do
        eval "(λx:String.λy:String.λz:String.z) \"a\"" `shouldBe` "(λy.(λz.z)):(String->(String->String))"

    it "(λx:String.λy:String.λz:String.z) \"a\" \"b\" -> (λz.z):(String->String)" $ do
        eval "(λx:String.λy:String.λz:String.z) \"a\" \"b\"" `shouldBe` "(λz.z):(String->String)"

    it "(λx:String.λy:String.λz:String.z) \"a\" \"b\" \"c\"-> (λz.z):(String->String)" $ do
        eval "(λx:String.λy:String.λz:String.z) \"a\" \"b\" \"c\"" `shouldBe` "c:String"

    it "(λx:Unit->Bool.if (x unit) then true else false)(λx:Unit.false) -> false:Bool" $ do
        eval "(λx:Unit->Bool.if (x unit) then true else false)(λx:Unit.false)" `shouldBe` "false:Bool"

    it "0 -> 0:Nat" $ do
        eval "0" `shouldBe` "0:Nat"

    it "(0) -> 0:Nat" $ do
        eval "(0)" `shouldBe` "0:Nat"

    it "((0)) -> 0:Nat" $ do
        eval "((0))" `shouldBe` "0:Nat"

    it "succ 0 -> succ 0:Nat" $ do
        eval "succ 0" `shouldBe` "succ 0:Nat"

    it "(succ 0) -> succ 0:Nat" $ do
        eval "(succ 0)" `shouldBe` "succ 0:Nat"

    it "((succ 0)) -> succ 0:Nat" $ do
        eval "((succ 0))" `shouldBe` "succ 0:Nat"

    it "isZero 0 -> true:Bool" $ do
        eval "isZero 0" `shouldBe` "true:Bool"

    it "(isZero 0) -> true:Bool" $ do
        eval "(isZero 0)" `shouldBe` "true:Bool"

    it "((isZero 0)) -> true:Bool" $ do
        eval "((isZero 0))" `shouldBe` "true:Bool"

    it "((isZero ((0)))) -> true:Bool" $ do
        eval "((isZero ((0))))" `shouldBe` "true:Bool"

    it "isZero (succ 0) -> false:Bool" $ do
        eval "isZero (succ 0)" `shouldBe` "false:Bool"

    it "pred (succ 0) -> 0:Nat" $ do
        eval "pred (succ 0)" `shouldBe` "0:Nat"

    it "pred succ 0 -> 0:Nat" $ do
        eval "pred succ 0" `shouldBe` "0:Nat"

    it "pred succ (0) -> 0:Nat" $ do
        eval "pred succ (0)" `shouldBe` "0:Nat"

    it "succ (pred (succ (0))) -> succ 0:Nat" $ do
        eval "succ (pred (succ (0)))" `shouldBe` "succ 0:Nat"

    it "succ (pred succ 0) -> succ 0:Nat" $ do
        eval "succ (pred succ 0)" `shouldBe` "succ 0:Nat"

    it "pred 0 -> 0:Nat" $ do
        eval "pred 0" `shouldBe` "0:Nat"

    it "succ pred pred pred pred 0 -> succ 0:Nat" $ do
        eval "succ pred pred pred pred 0" `shouldBe` "succ 0:Nat"

    it "succ (pred pred pred pred (0)) -> succ 0:Nat" $ do
        eval "succ (pred pred pred pred (0))" `shouldBe` "succ 0:Nat"

    it "if isZero 0 then succ 0 else succ pred 0 -> succ 0:Nat" $ do
        eval "if isZero 0 then succ 0 else succ pred 0" `shouldBe` "succ 0:Nat"

    it "if isZero succ 0 then succ 0 else pred 0 -> 0:Nat" $ do
        eval "if isZero succ 0 then succ 0 else pred 0" `shouldBe` "0:Nat"

    it "if isZero (succ 0) then succ 0 else 0 -> 0:Nat" $ do
        eval "if isZero (succ 0) then succ 0 else 0" `shouldBe` "0:Nat"

    it "if (succ 0) then succ 0 else 0 -> guard of conditional not a boolean 1:31" $ do
        eval "if (succ 0) then succ 0 else 0" `shouldBe` "guard of conditional not a boolean 1:31"

    it "if pred (succ 0) then succ 0 else 0 -> guard of conditional not a boolean 1:36" $ do
        eval "if pred (succ 0) then succ 0 else 0" `shouldBe` "guard of conditional not a boolean 1:36"

    it "if isZero (pred (succ 0)) then succ 0 else 0 -> succ 0:Nat" $ do
        eval "if isZero (pred (succ 0)) then succ 0 else 0" `shouldBe` "succ 0:Nat"

    it "if (isZero (pred (succ 0)) then succ 0 else 0 -> fail" $ do
        eval "if (isZero (pred (succ 0)) then succ 0 else 0" `shouldBe` "\"simple typed \955-calculus\" (line 1, column 27):\nunexpected \" \"\nexpecting \")\""

    it "0;true;false;if true then succ 0 else succ succ 0; succ 0; isZero 0; -> 0:Nat\ntrue:Bool\nfalse:Bool\nsucc 0:Nat" $ do
        eval "0;true;false;if true then succ 0 else succ succ 0; succ 0; isZero 0;" `shouldBe` "0:Nat\ntrue:Bool\nfalse:Bool\nsucc 0:Nat\nsucc 0:Nat\ntrue:Bool"

    it "isZero 0; isZero true -> true:Bool\nargument of isZero is not a number Bool" $ do
        eval "isZero 0; isZero true" `shouldBe` "true:Bool\nargument of isZero is not a number Bool"

    it "let x = succ succ 0 in x -> succ succ 0:Nat" $ do
        eval "let x = succ succ 0 in x" `shouldBe` "succ succ 0:Nat"

    it "let x = succ succ 0 in (λx:Nat. pred x) x -> succ 0:Nat" $ do
        eval "let x = succ succ 0 in (λx:Nat. pred x) x" `shouldBe` "succ 0:Nat"

    it "let x = succ succ 0 in (λz:Nat. pred x) 0 -> succ 0:Nat" $ do
        eval "let x = succ succ 0 in (λz:Nat. pred x) 0" `shouldBe` "succ 0:Nat"

    it "let x = (λx:Nat x Bool. if (isZero x.0) then true else false) in x {0,false} -> true:Bool" $ do
        eval "let x = (λx:Nat x Bool. if (isZero x.0) then true else false) in x {0,false}" `shouldBe` "true:Bool"

    it "true as Bool" $ do
        eval "true as Bool" `shouldBe` "true:Bool"

    it "false as Bool" $ do
        eval "false as Bool" `shouldBe` "false:Bool"

    it "\"foo\" as String" $ do
        eval "\"foo\" as String" `shouldBe` "foo:String"

    it "unit as Unit" $ do
        eval "unit as Unit" `shouldBe` "unit:Unit"

    it "1.1 as Float" $ do
        eval "1.1 as Float" `shouldBe` "1.1:Float"

    it "1.1000001 as Float" $ do
        eval "1.1000001 as Float" `shouldBe` "1.1000001:Float"

    it "{true, unit} as Bool x Unit" $ do
        eval "{true, unit} as Bool x Unit" `shouldBe` "{true,unit}:Bool x Unit"

    it "{true, unit}.0 as Bool" $ do
        eval "{true, unit}.0 as Bool" `shouldBe` "true:Bool"

    it "{true, unit}.1 as Unit" $ do
        eval "{true, unit}.1 as Unit" `shouldBe` "unit:Unit"

    it "(λx:Bool.true) as (Bool->Bool) -> (λx.true):(Bool->Bool)" $ do
        eval "(λx:Bool.true) as (Bool->Bool)" `shouldBe` "(λx.true):(Bool->Bool)"

    it "(λx:Bool.true) as (Bool->Bool) -> (λx.true):(Bool->Bool)" $ do
        eval "(λx:Bool.true) as (Bool->Bool)" `shouldBe` "(λx.true):(Bool->Bool)"

    it "(λx:Bool.true) as (Bool->Bool) -> (λx.true):(Bool->Bool)" $ do
        eval "(λx:Bool.true) as (Bool->Bool)" `shouldBe` "(λx.true):(Bool->Bool)"

    it "(λx:A.unit) -> (λx.unit):(A->Unit)" $ do
        eval "(λx:A.unit)" `shouldBe` "(λx.unit):(A->Unit)"

    it "(λx:A.λy:B.λz:C.true) -> (λx.(λy.(λz.true))):(A->(B->(C->Bool)))" $ do
        eval "(λx:A.λy:B.λz:C.true)" `shouldBe` "(λx.(λy.(λz.true))):(A->(B->(C->Bool)))"

    it "<a:unit> -> <a:unit>:<a:Unit>" $ do
        eval "<a:unit>" `shouldBe` "<a:unit>:<a:Unit>"

    it "<a:unit> as <a:Unit,b:Unit,c:Nat> -> <a:unit>:<a:Unit,b:Unit,c:Nat>" $ do
        eval "<a:unit> as <a:Unit,b:Unit,c:Nat>" `shouldBe` "<a:unit>:<a:Unit,b:Unit,c:Nat>"

    it "<a:true> as <a:Unit,b:Unit,c:Nat> -> type missmatch in a variant" $ do
        eval "<a:true> as <a:Unit,b:Unit,c:Nat>" `shouldBe` "type missmatch in a variant"

    it "<a:true> as <b:Unit,c:Nat> -> the annotated type <b:Unit,c:Nat> has't variant a" $ do
        eval "<a:true> as <b:Unit,c:Nat>" `shouldBe` "the annotated type <b:Unit,c:Nat> has't variant a"

    it "(λx:<a:Unit,b:Unit,c:Nat>.x) -> (λx.x):(<a:Unit,b:Unit,c:Nat>-><a:Unit,b:Unit,c:Nat>)" $ do
        eval "(λx:<a:Unit,b:Unit,c:Nat>.x)" `shouldBe` "(λx.x):(<a:Unit,b:Unit,c:Nat>-><a:Unit,b:Unit,c:Nat>)"

    it "let z = <a:true> as <a:Bool,b:Unit,c:Nat> in (λx:<a:Bool,b:Unit,c:Nat>.x) z -> <a:true>:<a:Bool,b:Unit,c:Nat>" $ do
        eval "let z = <a:true> as <a:Bool,b:Unit,c:Nat> in (λx:<a:Bool,b:Unit,c:Nat>.x) z" `shouldBe` "<a:true>:<a:Bool,b:Unit,c:Nat>"

    it "case <b:0> as <a:Bool,b:Nat> of <b:x> -> succ succ pred pred pred x | <a:y> -> 0 -> succ succ 0:Nat" $ do
        eval "case <b:0> as <a:Bool,b:Nat> of <b:x> -> succ succ pred pred pred x | <a:y> -> 0" `shouldBe` "succ succ 0:Nat"

    it "case <b:0> as <a:Bool,b:Nat> of <b:x> -> if (isZero x) then false else true | <a:y> -> true -> false:Bool" $ do
        eval "case <b:0> as <a:Bool,b:Nat> of <b:x> -> if (isZero x) then false else true | <a:y> -> true" `shouldBe` "false:Bool"

    it "case <b:succ 0> as <a:Nat,b:Nat> of <a:x> -> <a:succ x> | <b:y> -> <a:y>" $ do
        eval "case <b:succ 0> as <a:Nat,b:Nat> of <a:x> -> <a:succ x> | <b:y> -> <a:succ y>" `shouldBe` "<a:succ succ 0>:<a:Nat>"

    it "diverge" $ do
        eval "let d = (λx:Unit.fix (λy:T.y)) in d" `shouldBe` "(λx.(λy.y)):(Unit->T)"
