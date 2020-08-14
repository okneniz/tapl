{-# LANGUAGE FlexibleContexts #-}

module Language.TAPL.FullSimpleSpec where

import Test.Hspec
import Language.TAPL.FullSimple.Evaluator (evalString)

eval :: String -> String -> Either String String
eval = evalString

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
                  ("1", "1:Int"),
                  ("1.1000001", "1.1000001:Float")
                ]
           mapM_ test examples

        describe "pairs" $ do
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
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
           let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
               examples = [
                  ("{a=true, b=false}", "{a=true, b=false}:{a=Bool, b=Bool}"),
                  ("{a=true, b=unit}", "{a=true, b=unit}:{a=Bool, b=Unit}"),
                  ("{a=1.1, b=\"foo\"}", "{a=1.1, b=\"foo\"}:{a=Float, b=String}"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}", "{c=(lambda x.x), d=(lambda x.x)}:{c=(Unit -> Unit), d=(Unit -> Unit)}"),
                  ("{a=true, b=false}.a", "true:Bool"),
                  ("{a=true, b=unit}.b", "unit:Unit"),
                  ("{a=1.1, b=\"foo\"}.b", "\"foo\":String"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d", "(lambda x.x):(Unit -> Unit)"),
                  ("{c=(lambda x:Unit.x), d=((lambda x:Unit.x) unit)}.d", "unit:Unit"),
                  ("{c=(lambda x:Unit.x), d=(lambda x:Unit.x)}.d unit", "unit:Unit")
                ]
           mapM_ test examples

        describe "variants" $ do
          let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
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

        describe "abstractions" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
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
                    ("(lambda x:Bot.x)", "(lambda x.x):(Bot -> Bot)")
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
                  ("zero? succ zero", "false:Bool"),
                  ("timesfloat 1.0 2.0", "2.0:Float"),
                  ("timesfloat (timesfloat 1.3 1.111) 2.0", "2.8886000000000003:Float"),
                  ("timesfloat (timesfloat 3.14 2.0) 10.0", "62.800000000000004:Float")
                 ]
            mapM_ test examples

        describe "apply" $ do
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
                examples = [
                  ("(lambda x:Bool. if x then false else true) true", "false:Bool"),
                  ("(lambda x:Nat. succ x) zero", "succ zero:Nat"),
                  ("(lambda x:Bool -> Bool.lambda y:A. x true)", "(lambda x.(lambda y.x true)):((Bool -> Bool) -> (A -> Bool))"),
                  ("(lambda x:Nat. succ x) succ zero", "succ succ zero:Nat"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) pred zero", "(lambda y.(lambda z.if zero? zero then y else z)):(Float -> (Float -> Float))"),
                  ("(lambda x:Nat.lambda y:Float.lambda z:Float. if zero? x then y else z) zero 3.14 9.8", "3.14:Float"),
                  ("(lambda x:Nat -> Nat. x zero) (lambda x:Nat. succ x)", "succ zero:Nat"),
                  ("(lambda x:Nat -> Bool. \
                    \lambda y:Nat. \
                    \lambda z:Float. \
                    \lambda p:Float. \
                    \if x y then z else p) (lambda x:Nat. zero? x) (succ zero) 3.14",
                    "(lambda p.if (lambda x.zero? x) succ zero then 3.14 else p):(Float -> Float)"
                  ),
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
            let test = (\(x,y) -> it x $ do { eval x "<stdin>" `shouldBe` Right y })
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
                      "T = Nat->Nat",
                      ""
                    ),
                    (
                      "Bit = Bool; (lambda x:Bit.x)",
                      "(lambda x.x):(Bit -> Bit)"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda x:T.x zero) (lambda y:Nat. if zero? y then succ y else y)",
                      "succ zero:Nat"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda f:T. (lambda x:Nat. f x))",
                      "(lambda f.(lambda x.f x)):(T -> (Nat -> Nat))"
                    ),
                    (
                      "T = Nat->Nat; \
                      \ (lambda f:T. (lambda x:Nat. f (f x)))",
                      "(lambda f.(lambda x.f f x)):(T -> (Nat -> Nat))"
                    )
                 ]
            mapM_ test examples
