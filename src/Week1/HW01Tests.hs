-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module Week1.HW01Tests where

import Week1.HW01
import Week1.Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------
testtoRevDigits :: (Integer, [Integer]) -> Bool
testtoRevDigits (n, d) = toRevDigits n == d
ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits" testtoRevDigits
             [(341, [1,4,3]), (8341, [1,4,3,8]), (0, [])]]

-- Exercise 3 -----------------------------------------
testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, d) = doubleEveryOther n == d
ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther" testDoubleEveryOther
             [([1,2,3], [1,4,3]), ([1,2,3,4], [1,4,3,8]), ([], []), ([0], [0])]]

-- Exercise 4 -----------------------------------------
testsumDigits ::([Integer], Integer) -> Bool
testsumDigits (n, d) = sumDigits n == d
ex4Tests :: [Test]
ex4Tests = [Test "sumDigits" testsumDigits
             [([10, 5, 18, 4],19), ([1],1), ([], 0)]]

-- Exercise 5 -----------------------------------------
testluhn ::(Integer, Bool) -> Bool
testluhn (n, d) = luhn n == d

ex5Tests :: [Test]
ex5Tests = [Test "luhn" testluhn
             [(5594589764218858,True), (1234567898765432,False)]]

-- Exercise 6 -----------------------------------------
testhanoi ::(Integer, Peg ,Peg, Peg, [Move]) -> Bool
testhanoi (n, a, b, c, d) = hanoi n a b c == d
ex6Tests :: [Test]
ex6Tests = [Test "hanoi" testhanoi
             [(1, "a", "b", "c",[("a", "c")]),(2, "a","b","c",[("a", "b"), ("a", "c"), ("b", "c")])]]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]