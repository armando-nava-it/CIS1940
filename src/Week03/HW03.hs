module Week03.HW03
    ( extend
    , empty
    , evalE
    , desugar
    , evalSimple
    , run
    , slist
    , factorial
    , squareRoot
    , fibonacci
    , Expression (..)
    , Bop (..)
    , Statement (..)
    , DietStatement (..)
    ) where

data Expression
    = Var String -- Variable
    | Val Int -- Integer literal
    | Op Expression Bop Expression -- Operation
    deriving (Eq, Show)

-- Binary (2-input) operators
data Bop
    = Plus
    | Minus
    | Times
    | Divide
    | Gt
    | Ge
    | Lt
    | Le
    | Eql
    deriving (Eq, Show)

data Statement
    = Assign String Expression
    | Incr String
    | If Expression Statement Statement
    | While Expression Statement
    | For Statement Expression Statement Statement
    | Sequence Statement Statement
    | Skip
    deriving (Eq, Show)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st name val x = if x == name then val else st x

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE _ (Val n) = n
evalE st (Var x) = st x
evalE st (Op e1 bop e2) =
    let v1 = evalE st e1
        v2 = evalE st e2
     in case bop of
            Plus -> v1 + v2
            Minus -> v1 - v2
            Times -> v1 * v2
            Divide -> v1 `div` v2
            Gt -> boolToInt (v1 > v2)
            Ge -> boolToInt (v1 >= v2)
            Lt -> boolToInt (v1 < v2)
            Le -> boolToInt (v1 <= v2)
            Eql -> boolToInt (v1 == v2)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- Exercise 3 -----------------------------------------

data DietStatement
    = DAssign String Expression
    | DIf Expression DietStatement DietStatement
    | DWhile Expression DietStatement
    | DSequence DietStatement DietStatement
    | DSkip
    deriving (Eq, Show)

desugar :: Statement -> DietStatement
desugar snt = case snt of
    Skip -> DSkip
    Assign s e -> DAssign s e
    If e snt1 snt2 -> DIf e (desugar snt1) (desugar snt2)
    While e snt1 -> DWhile e (desugar snt1)
    Sequence snt1 snt2 -> DSequence (desugar snt1) (desugar snt2)
    Incr s -> DAssign s (Op (Var s) Plus (Val 1))
    For snt1 e snt2 snt3 ->
        DSequence
            (desugar snt1)
            ( DWhile
                e
                ( DSequence
                    (desugar snt3)
                    (desugar snt2)
                )
            )

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign name expr) =
    extend st name (evalE st expr)
evalSimple st (DIf cond s1 s2) =
    if evalE st cond /= 0
        then evalSimple st s1
        else evalSimple st s2
evalSimple st (DWhile cond body) =
    if evalE st cond /= 0
        then evalSimple (evalSimple st body) (DWhile cond body)
        else st
evalSimple st (DSequence s1 s2) =
    let st' = evalSimple st s1
     in evalSimple st' s2
evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial =
    For
        (Assign "Out" (Val 1))
        (Op (Var "In") Gt (Val 0))
        (Assign "In" (Op (Var "In") Minus (Val 1)))
        (Assign "Out" (Op (Var "In") Times (Var "Out")))

{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot =
    slist
        [ Assign "B" (Val 0)
        , While
            (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
            (Incr "B")
        , Assign "B" (Op (Var "B") Minus (Val 1))
        ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci =
    slist
        [ Assign "F0" (Val 1)
        , Assign "F1" (Val 1)
        , If
            (Op (Var "In") Eql (Val 0))
            (Assign "Out" (Var "F0"))
            ( If
                (Op (Var "In") Eql (Val 1))
                (Assign "Out" (Var "F1"))
                ( For
                    (Assign "C" (Val 2))
                    (Op (Var "C") Le (Var "In"))
                    (Incr "C")
                    ( slist
                        [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                        , Assign "F0" (Var "F1")
                        , Assign "F1" (Var "T")
                        , Assign "Out" (Var "T")
                        ]
                    )
                )
            )
        ]
