{-# LANGUAGE NondecreasingIndentation #-}

module HW04Tests (allTests) where

import Test.Hspec

import Week04.HW04

allTests :: Spec
allTests = describe "Week04" $ do
    describe "\nPoly Eq Instance" $ do
        it "Identical Poly " $ do
            P ([1, 2] :: [Int]) `shouldBe` P ([1, 2, 0] :: [Int])
        it "Non Identical Poly" $ do
            P ([1, 2] :: [Int]) `shouldNotBe` P ([0, 1, 2] :: [Int])

    describe "\nPoly Show Instance" $ do
        it "show (P ([0, 0, 0] :: [Int])) == 0" $ do
            show (P ([0, 0, 0] :: [Int])) `shouldBe` "0"
        it "show (P ([1, 0, 0, 2] :: [Int])) == '2x^3 + 1'" $ do
            show (P ([1, 0, 0, 2] :: [Int])) `shouldBe` "2x^3 + 1"
        it "show (P ([0, 1] :: [Int])) == 'x'" $ do
            show (P ([0, 1] :: [Int])) `shouldBe` "x"

    describe "\nPoly addition" $ do
        it "P [5, 0, 1] + P [1, 1, 2] == P [6, 1, 3]" $ do
            P ([5, 0, 1] :: [Int]) + P ([1, 1, 2] :: [Int]) `shouldBe` P ([6, 1, 3] :: [Int])
        it "P [1, 0, 1] + P [1, 1] == P [2, 1, 1]" $ do
            P ([1, 0, 1] :: [Int]) + P ([1, 1] :: [Int]) `shouldBe` P ([2, 1, 1] :: [Int])

    describe "\nPoly multiplication" $ do
        it "P [1, 1, 1] * P [2, 2] == P [2, 4, 4, 2]" $ do
            P ([1, 1, 1] :: [Int]) * P ([2, 2] :: [Int]) `shouldBe` P ([2, 4, 4, 2] :: [Int])

    describe "\nPoly subtraction" $ do
        it "P [1, 1, 1] - P [2, 2] == P [-1, -1, 1]" $ do
            P ([1, 1, 1] :: [Int]) - P ([2, 2] :: [Int]) `shouldBe` P ([-1, -1, 1] :: [Int])
        it "(1 + x + x^2) - (2 + 2x) == (x^2 + (-x) + (-1))" $ do
            ((x ^ (2 :: Int) :: Poly Int) + x + 1) - (2 * x + 2) `shouldBe` ((x ^ (2 :: Int) :: Poly Int) + (-x) + (-1))

    describe "\nPoly evaluation" $ do
        it "applyP (x^2 + 2*x + 1) 1 == 4" $ do
            applyP ((x ^ (2 :: Int) :: Poly Int) + 2 * x + 1) 1 `shouldBe` 4
        it "applyP (x^2 + 2*x + 1) 2 == 9" $ do
            applyP ((x ^ (2 :: Int) :: Poly Int) + 2 * x + 1) 2 `shouldBe` 9

    describe "\nPoly Differentiable Instance" $ do
        it "deriv (x^2 + 3*x + 5) == 2*x + 3" $ do
            deriv ((x ^ (2 :: Int) :: Poly Int) + 3 * x + 5) `shouldBe` 2 * x + 3
        it "nderiv 2 (x^2 + 3*x + 5 :: Poly Int) == 2" $ do
            nderiv 2 ((x ^ (2 :: Int) :: Poly Int) + 3 * x + 5 :: Poly Int) `shouldBe` 2
