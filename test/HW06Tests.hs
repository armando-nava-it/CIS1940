module HW06Tests (allTests) where

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
        describe "\n More efficient Fibonacci" $ do
            it "take 14 fibs2 == [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]" $ do
                take 14 fibs2 `shouldBe` [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]
        describe "\n Stream naturals" $ do
            it "sTake 4 nats == [1,2,3,4]" $ do
                sTake 4 nats `shouldBe` [1, 2, 3, 4]
        describe "\n Stream ruler" $ do
            it "sTake 16 ruler == [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]" $ do
                sTake 16 ruler `shouldBe` [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]
        describe "\n Stream rand" $ do
            it "minMax $ sTake 1000000 $ rand 7666532 == Just (1096,2147482927)" $ do
                minMax (sTake 1000000 $ rand 7666532) `shouldBe` Just (1096, 2147482927)
