module Week02.HW02 (
    module Week02.HW02
) where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
    deriving (Eq, Ord, Show)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
    deriving (Eq, Show)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = length . filter (== True) $ zipWith (==) a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors g = map (\color -> length $ filter (== color) g) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches s g = sum $ zipWith min sc gc
  where
    sc = countColors s
    gc = countColors g

-- Exercise 3 -----------------------------------------
{-
getMove [Red, Green, Yellow, Blue] [Red, Red, Blue, Green] == Move [Red, Red, Blue, Green] 1 1
(Move [Red, Red, Blue, Green] 1 1)
[Red, Blue, Red, Purple]
-}
-- >>> getMove [Red, Blue, Yellow, Orange] [Red, Red, Blue, Green]
-- Move [Red,Red,Blue,Green] 1 1

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move g em (m - em)
  where
    em = exactMatches s g
    m = matches s g

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent pm@(Move pc _ _) cc = getMove cc pc == pm

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes  m = filter (isConsistent m)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n
  | n < 0    = []
  | n == 0    = [[]] 
  | otherwise = concatMap (\x -> map (:x) colors) (allCodes $ n-1)

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = guesses $ allCodes $ length code
  where
    guesses :: [Code] -> [Move]
    guesses [] = []
    guesses (guess:rest)
                        | guess == code = [getMove code guess]
                        | otherwise     = move : guesses (filterCodes move rest)
                        where
                            move = getMove code guess

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
