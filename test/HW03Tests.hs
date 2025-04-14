-- CIS 194, Spring 2015
--
-- Test cases for HW 03

module HW03Tests (ex1Tests, allTests) where

import Week03.HW03
import Week1.Testing

-- Exercise 1 -----------------------------------------

ex1Tests :: [Test]
ex1Tests =
    [ testF4
        "extend test"
        extend
        [ (empty, "A", 5, "A", 5)
        , (empty, "A", 5, "B", 0)
        ]
    ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests =
    [ testF2
        "evalE test"
        evalE
        [ (empty, Val 5, 5)
        , (empty, Op (Val 1) Eql (Val 2), 0)
        ]
    ]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests =
    [ testF1
        "desugar test"
        desugar
        [(Incr "A", DAssign "A" (Op (Var "A") Plus (Val 1)))]
    ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests =
    [ testF3
        "evalSimple test"
        evalSimple
        [(empty, DAssign "A" (Val 10), "A", 10)]
    ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests =
    [ testF3
        "run test"
        run
        [ (extend empty "In" 4, factorial, "Out", 24)
        , (extend empty "A" 10, squareRoot, "B", 3)
        , (extend empty "In" 8, fibonacci, "Out", 34)
        ]
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
        ]
