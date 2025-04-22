{-# LANGUAGE InstanceSigs #-}

module Week04.HW04 (Poly (..), applyP, deriv, nderiv, x) where

import Data.List (dropWhileEnd, intercalate)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Eq a, Num a) => Eq (Poly a) where
    (P xs) == (P ys) = trim xs == trim ys
      where
        trim = dropWhileEnd (== 0)

-- Exercise 3 -----------------------------------------

instance (Eq a, Num a, Show a) => Show (Poly a) where
    show (P coeffs) =
        let terms = reverse $ zip [0 :: Int ..] coeffs
            shownTerms = map showTerm $ filter (\(_, c) -> c /= 0) terms
         in if null shownTerms
                then "0"
                else intercalate " + " shownTerms
      where
        showTerm (0, c) = show c
        showTerm (1, c)
            | c == 1 = "x"
            | otherwise = show c ++ "x"
        showTerm (e, c)
            | c == 1 = "x^" ++ show e
            | otherwise = show c ++ "x^" ++ show e

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P ys) = P (addLists xs ys)
  where
    addLists (a : as) (b : bs) = (a + b) : addLists as bs
    addLists [] bs = bs
    addLists as [] = as

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P []) _ = P []
times _ (P []) = P []
times (P xs) (P ys) = foldr plus (P []) shiftedProducts
  where
    shiftedProducts = [shift i (scale c ys) | (i, c) <- zip [0 ..] xs]

    scale c = map (c *)
    shift i cs = P (replicate i 0 ++ cs)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) :: Poly a -> Poly a -> Poly a
    (+) = plus
    (*) :: Poly a -> Poly a -> Poly a
    (*) = times
    negate :: Poly a -> Poly a
    negate (P coeffs) = P (map negate coeffs)
    fromInteger :: Integer -> Poly a
    fromInteger n = P [fromInteger n]

    -- No meaningful definitions exist
    abs = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P coeffs) x1 = sum $ zipWith (\a i -> a * x1 ^ i) coeffs [0 :: Int ..]

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f
        | n <= 0 = f
        | otherwise = deriv (nderiv (n - 1) f)

-- Exercise 9 -----------------------------------------

instance (Enum a, Num a) => Differentiable (Poly a) where
    deriv :: Poly a -> Poly a
    deriv (P []) = P []
    deriv (P (_ : coeffs)) =
        let derivCoeffs = zipWith (*) (map fromIntegral [1 :: Int ..]) coeffs
         in P derivCoeffs
