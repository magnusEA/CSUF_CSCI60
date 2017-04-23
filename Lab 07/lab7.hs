{-# OPTIONS_GHC -fdefer-type-errors #-}
-- Edgar Abundiz
-- lab7.hs
-- CSci 60, lab 7, programming component
--

{------------------------------Booleans in Haskell------------------------------

We will use the Bool type to represent Booleans in Haskell. This means that
True will correspond to 1, False to 0. The && operator will correspond to 
Boolean AND (i.e., xy in Haskell is `x && y`), || corresponds to OR (x+y becomes
`x || y`) and the `not` function will be used for Boolean negation. There is
not a built-in xor operator (or is there...) so part of this lab will be to 
build one. 
-}

{- Implementing Boolean functions as true tables: 
   We can use normal Haskell functions to implement Boolean functions described
   by truth tables. For example, the truth table for NAND is

    a   b  | not(a && b)
   --------+-------------
    0   0  |     1
    0   1  |     1
    1   0  |     1
    1   1  |     0

   which translates into the Haskell function:
-}
nand :: Bool -> Bool -> Bool
nand False False = True
nand False True  = True
nand True  False = True
nand True  True  = False

-- I.e., we basically just copy the table, translating 0 to False and 1 to True.

--------------------------------------------------------------------------------
-- Part 1: Implementing boolean functions

-- Use the truth table for Exclusive-Or to build the Haskell definition:
xor :: Bool -> Bool -> Bool
xor False False = False
xor False True = True
xor True False = False
xor True True = False
-- Use the truth table for Nor to implement it in Haskell:
nor :: Bool -> Bool -> Bool
nor False False = True
nor False True = False
nor True False = False
nor True True = False

-- Using the truth table for the Majority function you built (or will build) from the
-- writte part of the lab, implement the function in Haskell:
maj :: Bool -> Bool -> Bool -> Bool
maj False False False = False
maj False False True = False
maj False True False = False
maj False True True = True
maj True False False = False
maj True False True = True
maj True True False = True
maj True True True = True

--------------------------------------------------------------------------------
-- Part 2: Verifying Boolean identities. 
bools :: [Bool]
bools = [True, False]

{- We want to check some of the Boolean identities to make sure that they 
   actually hold. When we say something like 

     (a || b) == (b || a)             (OR is commutative)

   we mean that this property holds for *any* assignment of True/False to the
   variables a and b. For example

     (True || False) == (False || True)

   If we test this for *all* possible assignments to a and b (for two 
   variables there will be 2^2 = 4 possible assignments) and it is true for 
   all of them, then the identity holds.

   We can evaluate an expression over all possible combinations of Boolean
   assignments using list comprehensions. E.g., for two variables, we have

     [(a,b) | a <- bools, b <- bools]

   This will return the list of all possible pairs of Booleans

   If we replace the pair with the expression we want to test, we will get 
   back a list of Trues/Falses telling us whether the identity held for
   a particular assignment. E.g., for commutativity of OR:

     [(a || b) == (b || a) | a <- bools, b <- bools]

   If this identity holds, we would expect that the resulting list would contain
   nothing but True. We can test for this using the `and` function:

     and [(a || b) == (b || a) | a <- bools, b <- bools]

   This will return True if the identity held for all possible assignments. 
-}

-- Test the following identities for validity, by writing list comprehensions
-- as shown above. (Note that in the first two, the Haskell syntax is given,
-- but after that we use the normal Boolean expression syntax, which you will
-- have to translate into Haskell.)

-- This one is given:
-- Commutativity of OR
commutative_or :: Bool
commutative_or = and [(a || b) == (b || a) | a<- bools, b <- bools]

-- Commutativity of AND: (a && b) == (b && a)
commutative_and :: Bool
commutative_and = and [(a && b) == (b && a) | a<- bools, b <- bools]

-- Associativity of AND (a && (b && c)) == ((a && b) && c)
associative_and :: Bool
associative_and = and [(a && (b && c)) == ((a && b) && c) | a <- bools, b <- bools, c <- bools]

-- Associativity of OR: a + (b + c) == (a + b) + c
associative_or :: Bool
associative_or =  and [a || (b || c) == (a || b) || c | a <- bools, b <- bools, c <- bools]

-- AND distributes over OR: a(b + c) == ab + ac
and_distributes_or :: Bool
and_distributes_or = and [a &&(b || c) == a && b || a && c | a <- bools, b <- bools, c <- bools]

-- OR distributes over AND: a + bc = (a + b)(a + c)
or_distributes_and :: Bool
or_distributes_and = and [a || b && c == (a || b) && (a || c) | a <- bools, b <- bools, c <- bools]

-- Does NAND distribute over NOR? (Use your definition of `nor` from above.)
nand_distributes_nor :: Bool
nand_distributes_nor = False