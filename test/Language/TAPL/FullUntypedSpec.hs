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
                  ("timesfloat (timesfloat 3.14 2.0) 10.0", pass "62.800000000000004"),
                  ("timesfloat (timesfloat 3.14 2.0) (timesfloat 5.0 2.0)", pass "62.800000000000004")
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

        describe "Church Booleans" $ do
           let definitions = "\
               \let tru = (lambda t.(lambda f.t)) in \
               \let fls = (lambda t.(lambda f.f)) in \
               \let and = (lambda p.(lambda q.(p q) p)) in \
               \let or = (lambda p.(lambda q.(p p) q)) in \
               \let not = (lambda p.(lambda a.(lambda b.p b a))) in \
               \let xor = (lambda a.(lambda b.a (not b) b)) in \
               \let churchif = (lambda p.(lambda a.(lambda b.p a b))) in \
               \"

           tests evalString [
                    (
                      definitions ++ "fls",
                      pass "(lambda t.(lambda f.f))"
                    ),
                    (
                      definitions ++ "tru",
                      pass "(lambda t.(lambda f.t))"
                    ),

                    (
                      definitions ++ "and",
                      pass "(lambda p.(lambda q.p q p))"
                    ),
                    (
                      definitions ++ "and tru tru",
                      pass "(lambda t.(lambda f.t))"
                    ),
                    (
                      definitions ++ "and tru fls",
                      pass "(lambda t.(lambda f.f))"
                    ),
                    (
                      definitions ++ "and fls tru",
                      pass "(lambda t.(lambda f.f))"
                    ),
                    (
                      definitions ++ "and fls fls",
                      pass "(lambda t.(lambda f.f))"
                    ),

                    (
                      definitions ++ "or",
                      pass "(lambda p.(lambda q.p p q))"
                    ),
                    (
                      definitions ++ "or fls fls",
                      pass "(lambda t.(lambda f.f))"
                    ),
                    (
                      definitions ++ "or tru fls",
                      pass "(lambda t.(lambda f.t))"
                    ),
                    (
                      definitions ++ "or fls tru",
                      pass "(lambda t.(lambda f.t))"
                    ),
                    (
                      definitions ++ "or tru tru",
                      pass "(lambda t.(lambda f.t))"
                    ),

                    (
                      definitions ++ "not",
                      pass "(lambda p.(lambda a.(lambda b.p b a)))"
                    ),
                    (
                      definitions ++ "(not tru) true false",
                      pass "false"
                    ),
                    (
                      definitions ++ "(not fls) true false",
                      pass "true"
                    ),

                    (
                      definitions ++ "xor",
                      pass "(lambda a.(lambda b.a (lambda p.(lambda a'.(lambda b'.p b' a'))) b b))"
                    ),
                    (
                      definitions ++ "(xor tru tru) true false",
                      pass "false"
                    ),
                    (
                      definitions ++ "(xor tru fls) true false",
                      pass "true"
                    ),
                    (
                      definitions ++ "(xor fls tru) true false",
                      pass "true"
                    ),
                    (
                      definitions ++ "(xor fls fls) true false",
                      pass "false"
                    ),

                    (
                      definitions ++ "churchif",
                      pass "(lambda p.(lambda a.(lambda b.p a b)))"
                    ),
                    (
                      definitions ++ "(churchif tru) true false",
                      pass "true"
                    ),
                    (
                      definitions ++ "(churchif fls) true false",
                      pass "false"
                    )
                 ]

        describe "Church numbers" $ do
           let definitions = "\
               \let z =           (lambda f.(lambda x.x)) in \
               \let one =         (lambda f.(lambda x.f x)) in \
               \let two =         (lambda f.(lambda x.f (f x))) in \
               \let three =       (lambda f.(lambda x.f (f (f x)))) in \
               \let four =        (lambda f.(lambda x.f (f (f (f x))))) in \
               \let five =        (lambda f.(lambda x.f (f (f (f (f x)))))) in \
               \let six =         (lambda f.(lambda x.f (f (f (f (f (f x))))))) in \
               \let seven =       (lambda f.(lambda x.f (f (f (f (f (f (f x)))))))) in \
               \let eight =       (lambda f.(lambda x.f (f (f (f (f (f (f (f x))))))))) in \
               \let nine =        (lambda f.(lambda x.f (f (f (f (f (f (f (f (f x)))))))))) in \
               \let ten =         (lambda f.(lambda x.f (f (f (f (f (f (f (f (f (f x))))))))))) in \
               \let plus =        (lambda m.(lambda n.(lambda f.(lambda x.m f (n f x))))) in \
               \let successor =   (lambda n.(lambda f.(lambda x.n f x))) in \
               \let multiply =    (lambda m.(lambda n.(lambda f.(lambda x.(m (n f)) x)))) in \
               \let exp =         (lambda m.(lambda n.m n)) in \
               \let cube =        exp two in \
               \let predecessor = (lambda n.(lambda f.(lambda x.n (lambda g.(lambda h.h (g f))) (lambda u.x) (lambda u.u)))) in \
               \let minus =       (lambda m.(lambda n.(n predecessor) m)) in \
               \"

           tests evalString [
                    (
                      definitions ++ "plus z one",
                      pass "(lambda f.(lambda x.(lambda f'.(lambda x'.x')) f (lambda f'.(lambda x'.f' x')) f x))"
                    ),
                    (
                      definitions ++ "successor z",
                      pass "(lambda f.(lambda x.(lambda f'.(lambda x'.x')) f x))"
                    ),
                    (
                      definitions ++ "successor one",
                      pass "(lambda f.(lambda x.(lambda f'.(lambda x'.f' x')) f x))"
                    ),
                    (
                      definitions ++ "multiply two five",
                      pass "(lambda f.(lambda x.(lambda f'.(lambda x'.f' f' x')) (lambda f'.(lambda x'.f' f' f' f' f' x')) f x))"
                    ),

                    (
                      definitions ++ "z (lambda x.timesfloat x 2.0) 3.0",
                      pass "3.0"
                    ),
                    (
                      definitions ++ "one (lambda x.timesfloat x 2.0) 3.0",
                      pass "6.0"
                    ),
                    (
                      definitions ++ "two (lambda x.timesfloat x 2.0) 3.0",
                      pass "12.0"
                    ),
                    (
                      definitions ++ "three (lambda x.timesfloat x 2.0) 3.0",
                      pass "24.0"
                    ),
                    (
                      definitions ++ "four (lambda x.timesfloat x 2.0) 3.0",
                      pass "48.0"
                    ),
                    (
                      definitions ++ "five (lambda x.timesfloat x 2.0) 3.0",
                      pass "96.0"
                    ),
                    (
                      definitions ++ "six (lambda x.timesfloat x 2.0) 3.0",
                      pass "192.0"
                    ),
                    (
                      definitions ++ "seven (lambda x.timesfloat x 2.0) 3.0",
                      pass "384.0"
                    ),
                    (
                      definitions ++ "eight (lambda x.timesfloat x 2.0) 3.0",
                      pass "768.0"
                    ),
                    (
                      definitions ++ "nine (lambda x.timesfloat x 2.0) 3.0",
                      pass "1536.0"
                    ),
                    (
                      definitions ++ "ten (lambda x.timesfloat x 2.0) 3.0",
                      pass "3072.0"
                    ),

                    (
                      definitions ++ "z two (lambda x.timesfloat x 2.0) 3.0",
                      pass "6.0"
                    ),
                    (
                      definitions ++ "z ten (lambda x.timesfloat x 2.0) 3.0",
                      pass "6.0"
                    ),

                    (
                      definitions ++ "successor one (lambda x.timesfloat x 2.0) 3.0",
                      pass "6.0"
                    ),

                    (
                      definitions ++ "predecessor two (lambda x.timesfloat x 2.0) 3.0",
                      pass "6.0"
                    ),

                    (
                      definitions ++ "((multiply three two) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),
                    (
                      definitions ++ "((multiply two three) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),

                    (
                      definitions ++ "((plus z six) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),
                    (
                      definitions ++ "((plus three (plus one (plus two z))) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),
                    (
                      definitions ++ "((plus three (plus one (successor two))) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),
                    (
                      definitions ++ "((minus seven one) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),
                    (
                      definitions ++ "((minus eight two) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),
                    (
                      definitions ++ "((minus ten four) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "192.0"
                    ),

                    (
                      definitions ++ "((multiply two two) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "48.0"
                    ),
                    (
                      definitions ++ "((exp two two) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "48.0"
                    ),
                    (
                      definitions ++ "((cube two) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "48.0"
                    ),

                    (
                      definitions ++ "((multiply two (multiply two two)) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "768.0"
                    ),
                    (
                      definitions ++ "((exp three two) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "768.0"
                    ),
                    (
                      definitions ++ "((exp three (predecessor three)) (lambda x.timesfloat x 2.0)) 3.0",
                      pass "768.0"
                    )
                 ]

        describe "Church lists" $ do
           let definitions = "\
               \let pair   = (lambda x.(lambda y.(lambda z.z x y))) in \
               \let first  = (lambda p.p (lambda x.(lambda y.x))) in \
               \let second = (lambda p.p (lambda x.(lambda y.y))) in \
               \let head   = first in \
               \let tail   = second in \
               \let cons   = pair in \
               \"

           tests evalString [
                    (
                      definitions ++ "first (pair 3.14 0.0)",
                      pass "3.14"
                    ),
                    (
                      definitions ++ "second (pair 3.14 0.0)",
                      pass "0.0"
                    ),
                    (
                      definitions ++ "head (cons 3.14 0.0)",
                      pass "3.14"
                    ),
                    (
                      definitions ++ "head (cons 1.1 (cons 3.14 0.0))",
                      pass "1.1"
                    ),
                    (
                      definitions ++ "tail (cons 3.14 0.0)",
                      pass "0.0"
                    ),
                    (
                      definitions ++ "tail (cons 1.1 (cons 3.14 0.0))",
                      pass "(lambda z.z 3.14 0.0)"
                    ),
                    (
                      definitions ++ "tail (tail (cons 1.1 (cons 3.14 0.0)))",
                      pass "0.0"
                    ),
                    (
                      definitions ++ "head (tail (cons 1.1 (cons 3.14 0.0)))",
                      pass "3.14"
                    )
                 ]
