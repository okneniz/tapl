module Language.TAPL.TestHelpers where

import Test.Hspec

pass :: String -> Either String String -> Expectation
pass output result = result `shouldBe` (Right output)

failed :: String -> Either String String -> Expectation
failed output result = result `shouldBe` (Left output)

test f input check = specify input $ do { check $ f input }

tests f examples = do { mapM_ (uncurry $ test f) examples }
