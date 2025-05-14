{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Week07.HW07
    ( Week07.HW07.liftM
    , swapV
    , Week07.HW07.mapM
    , mapM''
    , getElts
    , randomElt
    , randomVec
    , randomVecR
    , shuffle
    , partitionAt
    , qsort
    , quicksort
    , qsortR
    , select
    , allCards
    , newDeck
    , nextCard
    , getCards
    , repl
    , main
    ) where

import Control.Monad hiding (liftM, mapM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, (!), (!?), (//))
import qualified Data.Vector as V
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

-- Alternative solution with explicit use of bind
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

-- Alternative solution using do notation
-- randomVec n = do
--   xs <- replicateM n getRandom
--   return $ V.fromList xs

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = Week07.HW07.liftM V.fromList $ replicateM n $ getRandomR r

-- Alternative solution using do notation
-- randomVecR n range = do
--   xs <- replicateM n (getRandomR r)
--   return $ V.fromList xs

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = do
    let n = V.length vec
    swaps <- sequence [ do
                          j <- getRandomR (0, i)
                          return (i, j)
                      | i <- [n-1, n-2 .. 1]
                      ]
    return (applySwaps vec swaps)


applySwaps :: Vector a -> [(Int, Int)] -> Vector a
applySwaps = foldl (\v (i, j) -> v // [(i, v ! j), (j, v ! i)])

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec idx =
    let pivot = vec ! idx
        withoutPivot = V.ifilter (\i _ -> i /= idx) vec
        less = V.filter (< pivot) withoutPivot
        greaterOrEqual = V.filter (>= pivot) withoutPivot
    in (less, pivot, greaterOrEqual)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
    quicksort [ y | y <- xs, y < x]
        <> (x : quicksort [ y | y <- xs, y >= x])

qsort :: Ord a => Vector a -> Vector a
qsort vec
  | V.null vec = V.empty
  | otherwise  =
      let pivotIndex = 0
          (less, pivot, greaterOrEqual) = partitionAt vec pivotIndex
      in qsort less V.++ V.singleton pivot V.++ qsort greaterOrEqual

-- Alternative solution using e Monad Comprehension
-- qsort :: Ord a => Vector a -> Vector a
-- qsort vec
--   | null vec = vec
--   | otherwise = qsort [ y | y <- xs, y < x ] <> cons x (qsort [ y | y <- xs, y >= x ])
--     where
--       x = V.head vec
--       xs = V.tail vec

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec
  | V.null vec = return V.empty
  | otherwise = do
      let n = V.length vec
      pivotIndex <-  getRandomR (0, n - 1)
      let (less, pivot, greaterOrEqual) = partitionAt vec pivotIndex
      leftSorted  <- qsortR less
      rightSorted <- qsortR greaterOrEqual
      return $ leftSorted V.++ V.singleton pivot V.++ rightSorted
      

-- Alternative solution with explicit use of bind 
-- qsortR :: Ord a => Vector a -> Rnd (Vector a)
-- qsortR v
--   | V.null v = return V.empty
--   | otherwise = parts >>= mergeSort
--   where
--     parts = getRandomR (0, V.length v - 1) >>= return . partitionAt v
--     mergeSort (b, p, t) = liftM2 (<>) (qsortR b) (fmap (cons p) (qsortR t))


-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i vec
  | i < 0 || i >= V.length vec = return Nothing
  | otherwise = do
      let n = V.length vec
      pivotIndex <- getRandomR (0, n - 1)
      let (less, pivot, greaterOrEqual) = partitionAt vec pivotIndex
          lenLess = V.length less
      case compare i lenLess of
        LT -> select i less
        EQ -> return (Just pivot)
        GT -> select (i - lenLess - 1) greaterOrEqual

-- Alternative solution with explicit use of bind 
-- select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
-- select i v
--   | V.null v = return Nothing
--   | otherwise = liftM (partitionAt v) rpi >>= helper
--   where
--     rpi = getRandomR (0, V.length v - 1)
--     helper (bottom, pivot, top)
--       | i < blen = select i bottom
--       | i > blen = select (i - blen - 1) top
--       | otherwise = return $ Just pivot
--       where blen = V.length bottom

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card l s | s <- suits, l <- labels ]

newDeck :: Rnd Deck
newDeck = shuffle allCards 

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.null d = Nothing
  | otherwise = return (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n deck = go n ([], deck)
  where
    go :: Int -> ([Card], Deck) -> Maybe ([Card], Deck)
    go 0 ret = return ret
    go m (crds, d) = nextCard d >>= \(crd, r) -> go (m-1) (crd:crds, r)

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
