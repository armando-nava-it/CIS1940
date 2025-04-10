-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module Week1.HW01Tests
    ( module Week1.HW01Tests
    ) where

import Week1.HW01
import Week1.Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

ex1Tests :: [Test]
ex1Tests =
    [ Test
        "lastDigit test"
        testLastDigit
        [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
    , Test
        "dropLastDigit test"
        testDropLastDigit
        [(123, 12), (1234, 123), (5, 0), (10, 1), (0, 0)]
    ]

-- Exercise 2 -----------------------------------------
ex2Tests :: [Test]
ex2Tests =
    [ testF1
        "toRevDigits"
        toRevDigits
        [(341, [1, 4, 3]), (8341, [1, 4, 3, 8]), (0, [])]
    ]

-- Exercise 3 -----------------------------------------
ex3Tests :: [Test]
ex3Tests =
    [ testF1
        "doubleEveryOther"
        doubleEveryOther
        [([1, 2, 3], [1, 4, 3]), ([1, 2, 3, 4], [1, 4, 3, 8]), ([], []), ([0], [0])]
    ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests =
    [ testF1
        "sumDigits"
        sumDigits
        [([10, 5, 18, 4], 19), ([1], 1), ([], 0)]
    ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests =
    [ testF1
        "luhn"
        luhn
        [(5594589764218858, True), (1234567898765432, False)]
    ]

-- Exercise 6 -----------------------------------------
testhanoi :: (Integer, Peg, Peg, Peg, [Move]) -> Bool
testhanoi (n, a, b, c, d) = hanoi n a b c == d
ex6Tests :: [Test]
ex6Tests =
    [ Test
        "hanoi"
        testhanoi
        [(1, "a", "b", "c", [("a", "c")]), (2, "a", "b", "c", [("a", "b"), ("a", "c"), ("b", "c")])]
    ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests =
    concat
        [ ex1Tests
        , ex2Tests
        , ex3Tests
        , ex4Tests
        , ex5Tests
        , ex6Tests
        ]
