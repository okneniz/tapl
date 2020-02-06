module Language.TAPL.EquirecSpec where

import Test.Hspec
import Language.TAPL.Equirec.Evaluator (evalString)

spec :: Spec
spec = do
  describe "eval" $ do
    describe "abstractions" $ do
      let test = (\(x,y) -> it x $ do { evalString x "<stdin>" `shouldBe` Right y })
          examples = [
              ("(lambda x:A.x)", "(lambda x.x):A -> A"),
              ("(lambda f:Rec X.A->A.lambda x:A. f x)", "(lambda f.(lambda x.f x)):Rec X.A -> A -> A -> A")
           ]
      mapM_ test examples
