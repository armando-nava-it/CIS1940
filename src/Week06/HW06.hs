{-# LANGUAGE BangPatterns #-}

module Week06.HW06
    ( fib
    , fibs1
    , fibs2
    , sTake
    , sRepeat
    , sIterate
    , sInterleave
    , nats
    , ruler
    , rand
    , fastFib
    , minMax
    , minMaxSlow
    , minMax'
    , main
    ) where

import Data.List (foldl', intercalate)

-- Exercise 1 -----------------------------------------

-- 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610
--    1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s =
        "["
            ++ intercalate ", " (map show $ take 10 $ streamToList s)
            ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x1 xs1) s2 = Cons x1 (sInterleave s2 xs1)

sTake :: Int -> Stream a -> [a]
sTake n (Cons x xs)
    | n < 1 = []
    | otherwise = x : sTake (n - 1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (1 +) 1

ruler :: Stream Integer
ruler = buildRuler 0
  where
    buildRuler :: Integer -> Stream Integer
    buildRuler n = sInterleave (sRepeat n) (buildRuler (n + 1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate next
  where
    next x = (1103515245 * x + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 83 MiB total memory in use (0 MiB lost due to fragmentation) -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 6 MiB total memory in use (0 MiB lost due to fragmentation) -}

minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x : xs) = Just (foldl' step (x, x) xs)
  where
    step (!mi, !ma) y = (min mi y, max ma y)

{- Total Memory in use: 6 MiB total memory in use (0 MiB lost due to fragmentation) -}

minMax' :: [Int] -> Maybe (Int, Int)
minMax' [] = Nothing
minMax' (x : xs) = Just $ helper xs x x
  where
    helper [] mn mx = (mn, mx)
    helper (y : ys) !mn !mx = helper ys (min y mn) (max y mx)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
