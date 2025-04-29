module HW06Tests (allTests) where

-- import System.Directory
import Test.Hspec

import Week06.HW06

allTests :: Spec
allTests = describe "\nWeek06" $ do
    describe "\nFibonacci" $ do
        describe "\n Simple Fibonacci" $ do
            it "fib 1 = 1" $ do
                fib 1 `shouldBe` 1
            it "fib 12 = 233" $ do
                fib 12 `shouldBe` 233
