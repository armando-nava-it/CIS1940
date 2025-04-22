module HW04Tests (allTests) where

import Test.Hspec

import Week04.HW04

p :: [Int] -> Poly Int
p = P
allTests :: Spec
allTests = describe "Week04" $ do
    describe "\nPoly Eq Instance" $ do
        it "Identical Poly " $ do
            p [1, 2] `shouldBe` p [1, 2, 0]
        it "Non Identical Poly" $ do
            p [1, 2] `shouldNotBe` p [0, 1, 2]

    describe "\nPoly Show Instance" $ do
        it "show (p [0, 0, 0] )) == 0" $ do
            show (p [0, 0, 0]) `shouldBe` "0"
        it "show (p [1, 0, 0, 2] )) == '2x^3 + 1'" $ do
            show (p [1, 0, 0, 2]) `shouldBe` "2x^3 + 1"
        it "show (p [0, 1] )) == 'x'" $ do
            show (p [0, 1]) `shouldBe` "x"

    describe "\nPoly addition" $ do
        it "p [5, 0, 1] + p [1, 1, 2] == p [6, 1, 3]" $ do
            p [5, 0, 1] + p [1, 1, 2] `shouldBe` p [6, 1, 3]
        it "p [1, 0, 1] + p [1, 1] == p [2, 1, 1]" $ do
            p [1, 0, 1] + p [1, 1] `shouldBe` p [2, 1, 1]

    describe "\nPoly multiplication" $ do
        it "p [1, 1, 1] * p [2, 2] == p [2, 4, 4, 2]" $ do
            p [1, 1, 1] * p [2, 2] `shouldBe` p [2, 4, 4, 2]

    describe "\nPoly subtraction" $ do
        it "p [1, 1, 1] - p [2, 2] == p [-1, -1, 1]" $ do
            p [1, 1, 1] - p [2, 2] `shouldBe` p [-1, -1, 1]
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
            nderiv 2 ((x ^ (2 :: Int) :: Poly Int) + 3 * x + 5) `shouldBe` 2
