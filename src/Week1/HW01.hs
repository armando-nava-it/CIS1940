module Week1.HW01
    ( module Week1.HW01
    ) where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = flip mod 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x <= 0 = []
    | otherwise = lastDigit x : toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1, 2])

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toRevDigits xs)

-- Exercise 5 -----------------------------------------

luhn :: Integer -> Bool
luhn x = lastDigit (sumDigits (doubleEveryOther (toRevDigits x))) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n < 1 = []
    | n == 1 = [(a, c)]
    | otherwise = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) b a c
