module HW07Tests (allTests) where

import Test.Hspec

import Week07.HW07
import qualified Data.Vector as V

allTests :: Spec
allTests = describe "\nWeek07" $ do
    describe "\n Monands " $ do
        
        describe "\n liftM implementation " $ do
            it " liftM (+1) (Just 5) == Just 6" $ do
                Week07.HW07.liftM (+(1 ::Int)) (Just 5) `shouldBe` Just 6
        
        describe "\n swapV implementation " $ do
            it "swapV 0 2 (V.fromList [1, 2, 3]) == Just (V.fromList [3, 2, 1])" $ do
                swapV 0 2 (V.fromList [1 ::Int, 2, 3]) `shouldBe` Just (V.fromList [3, 2, 1])
            it "swapV 0 2 (V.fromList [1, 2]) == Nothing" $ do
                swapV 0 2 (V.fromList [1 ::Int, 2]) `shouldBe` Nothing

        describe "\n mapM implementation " $ do
            it "mapM Just [1..10] == Just [1..10]" $ do
                Week07.HW07.mapM Just [1..10 ::Int] `shouldBe` Just [1..10]
            it "mapM'' Just [1..10] == Just [1..10]" $ do
                mapM'' Just [1..10::Int] `shouldBe` Just [1..10] 

        describe "\n getElts implementation " $ do
            it "getElts [1,3] (V.fromList [0..9]) == Just [1, 3]" $ do
                getElts [1,3] (V.fromList [0..9::Int]) `shouldBe` Just [1, 3]
            it "getElts [13,11] (V.fromList [0..9]) == Nothing" $ do
                getElts [13,11] (V.fromList [0..9::Int]) `shouldBe` Nothing