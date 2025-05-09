{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Week07.HW07
    ( Week07.HW07.liftM
    , swapV
    , Week07.HW07.mapM
    , mapM''
    , getElts
    ) where

import Control.Monad hiding (liftM, mapM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import System.Random
import Week07.Cards
import Prelude hiding (mapM)

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = ma >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 v = do
    x <- v !? i1
    y <- v !? i2
    return $ v // [(i1, y), (i2, x)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f l = sequence $ map f l

mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'' _ [] = return []
mapM'' f (x:xs) = do
  y <- f x
  ys <- mapM'' f xs
  return $ y:ys

-- f x >>= \y -> mapM'' f xs >>= \ys -> return $ y:ys  

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v = Week07.HW07.mapM (v!?) l

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = getRandomR (0, V.length v - 1) <&> (v !?)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = Week07.HW07.liftM V.fromList $ replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = Week07.HW07.liftM V.fromList $ replicateM n $ getRandomR r
-- >>> randomVecR 3 (1,4)

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle = undefined

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt = undefined

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
    quicksort [ y | y <- xs, y < x]
        <> (x : quicksort [ y | y <- xs, y >= x])

qsort :: Ord a => Vector a -> Vector a
qsort = undefined

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR = undefined

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck = undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State {money :: Int, deck :: Deck}

repl :: State -> IO ()
repl s@State{..}
    | money <= 0 = putStrLn "You ran out of money!"
    | V.null deck = deckEmpty
    | otherwise = do
        putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
        putStrLn "Would you like to play (y/n)?"
        cont <- getLine
        if cont == "n"
            then
                putStrLn $
                    "You left the casino with \ESC[32m$"
                        ++ show money
                        ++ "\ESC[0m"
            else play
  where
    deckEmpty =
        putStrLn $
            "The deck is empty. You got \ESC[32m$"
                ++ show money
                ++ "\ESC[0m"
    play = do
        putStrLn "How much do you want to bet?"
        amt <- read <$> getLine
        if amt < 1 || amt > money
            then play
            else do
                case getCards 2 deck of
                    Just ([c1, c2], d) -> do
                        putStrLn $ "You got:\n" ++ show c1
                        putStrLn $ "I got:\n" ++ show c2
                        case () of
                            _
                                | c1 > c2 -> repl $ State (money + amt) d
                                | c1 < c2 -> repl $ State (money - amt) d
                                | otherwise -> war s{deck = d} amt
                    _ -> deckEmpty
    war (State m d) amt = do
        putStrLn "War!"
        case getCards 6 d of
            Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                    _
                        | c13 > c23 -> repl $ State (m + amt) d'
                        | c13 < c23 -> repl $ State (m - amt) d'
                        | otherwise -> war (State m d') amt
            _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
