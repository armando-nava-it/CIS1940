{-# LANGUAGE ExistentialQuantification #-}

module Week1.Testing
    ( testF1
    , testF2
    , testF3
    , Test (..)
    , runTest
    , runTests
    ) where

import Control.Arrow
import Data.Maybe

data Test = forall a. Show a => Test String (a -> Bool) [a]
data Failure = forall a. Show a => Fail String [a]

instance Show Failure where
    show (Fail s as) =
        "Failed Test \""
            ++ s
            ++ "\" on inputs "
            ++ show as

runTest :: Test -> Maybe Failure
runTest (Test s f as) = case filter (not . f) as of
    [] -> Nothing
    fs -> Just $ Fail s fs

runTests :: [Test] -> [Failure]
runTests = mapMaybe runTest

-- Helpers

testF1 :: (Eq b, Show b) => String -> (a -> b) -> [(a, b)] -> Test
testF1 s f l = Test s (uncurry (==)) $ map (first f) l

testF2 ::
    (Eq c, Show c) =>
    String ->
    (a -> b -> c) ->
    [(a, b, c)] ->
    Test
testF2 s f l = Test s (uncurry (==)) $ map (\(x, y, z) -> (f x y, z)) l

testF3 ::
    (Eq d, Show d) =>
    String ->
    (a -> b -> c -> d) ->
    [(a, b, c, d)] ->
    Test
testF3 s f l = Test s (uncurry (==)) $ map (\(w, x, y, z) -> (f w x y, z)) l
