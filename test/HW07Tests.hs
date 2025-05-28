module HW07Tests (allTests) where

import Control.Monad.Random
import qualified Data.Vector as V
import Test.Hspec

import Week07.Cards
import Week07.HW07

testRand :: Rand StdGen a -> a
testRand action = evalRand action (mkStdGen 4)

allTests :: Spec
allTests = describe "\nWeek07" $ do
    describe "\n Monands " $ do
        describe "\n liftM implementation " $ do
            it " liftM (+1) (Just 5) == Just 6" $ do
                Week07.HW07.liftM (+ (1 :: Int)) (Just 5) `shouldBe` Just 6

        describe "\n swapV implementation " $ do
            it "swapV 0 2 (V.fromList [1, 2, 3]) == Just (V.fromList [3, 2, 1])" $ do
                swapV 0 2 (V.fromList [1 :: Int, 2, 3]) `shouldBe` Just (V.fromList [3, 2, 1])
            it "swapV 0 2 (V.fromList [1, 2]) == Nothing" $ do
                swapV 0 2 (V.fromList [1 :: Int, 2]) `shouldBe` Nothing

        describe "\n mapM implementation " $ do
            it "mapM Just [1..10] == Just [1..10]" $ do
                Week07.HW07.mapM Just [1 .. 10 :: Int] `shouldBe` Just [1 .. 10]
            it "mapM'' Just [1..10] == Just [1..10]" $ do
                mapM'' Just [1 .. 10 :: Int] `shouldBe` Just [1 .. 10]

        describe "\n getElts implementation " $ do
            it "getElts [1,3] (V.fromList [0..9]) == Just [1, 3]" $ do
                getElts [1, 3] (V.fromList [0 .. 9 :: Int]) `shouldBe` Just [1, 3]
            it "getElts [13,11] (V.fromList [0..9]) == Nothing" $ do
                getElts [13, 11] (V.fromList [0 .. 9 :: Int]) `shouldBe` Nothing

        describe "\n picks random element from a vector " $ do
            it "randomElt (V.fromList [0..9]) == Just 7" $ do
                testRand (randomElt (V.fromList [0 .. 9])) `shouldBe` Just (7 :: Int)

        describe "\n generates a random vector with n element Int " $ do
            it "randomVec 2 == [7758512093868915741,3565601448639051278]" $ do
                testRand (randomVec 2 :: Rand StdGen (V.Vector Int)) `shouldBe` V.fromList [7758512093868915741, 3565601448639051278]

        describe "\n generates a random vector with n element Int from a range " $ do
            it "randomVecR 2 (1,10) == [8,4]" $ do
                testRand (randomVecR 2 (1, 10) :: Rand StdGen (V.Vector Int)) `shouldBe` V.fromList [8, 4]

        describe "\n shuffles the elements of a vector " $ do
            it "shuffle (V.fromList [1,2,3,4,5,6]) == [1,3,2,5,4,6]" $ do
                testRand (shuffle (V.fromList [1, 2, 3, 4, 5, 6]) :: Rand StdGen (V.Vector Int)) `shouldBe` V.fromList [1, 3, 2, 5, 4, 6]

        describe "\n QuickSort implementation for Vectors" $ do
            it "qsort (V.fromList [1,4,6,5,2,3]) == V.fromList [1,2,3,4,5,6]" $ do
                qsort (V.fromList [1, 4, 6, 5, 2, 3 :: Int]) `shouldBe` V.fromList [1, 2, 3, 4, 5, 6]

        describe "\n QuickSort implementation for Vectors with a random Pivot" $ do
            it "qsortR (V.fromList [1,5,7,8,3,2,6,4]) == V.fromList [1,2,3,4,5,6,7,8]" $ do
                testRand (qsortR (V.fromList [1, 5, 7, 8, 3, 2, 6, 4 :: Int])) `shouldBe` V.fromList [1, 2, 3, 4, 5, 6, 7, 8]

        describe "\n selects a value with rank i from a Vector" $ do
            it "select 3 (V.fromList [1,2,3,4,5,6,7,8]) == Just 4" $ do
                testRand (select 3 (V.fromList [1, 2, 3, 4, 5, 6, 7, 8 :: Int])) `shouldBe` Just 4

        describe "\n Cards" $ do
            it "draw a card from a deck" $ do
                let (result, newdeck) =
                        testRand
                            ( do
                                deck <- newDeck
                                return (nextCard deck, deck)
                            )
                fmap fst result `shouldBe` Just (Card Two Heart)
                fmap snd result `shouldBe` Just (V.tail newdeck)
