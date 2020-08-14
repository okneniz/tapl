module Language.TAPL.TestHelpers where

import Test.Hspec

pass :: String -> Either String String -> Expectation
pass output result = result `shouldBe` (Right output)

test f input check = specify input $ do { check $ f input }

tests f examples = do { mapM_ (uncurry $ test f) examples }
