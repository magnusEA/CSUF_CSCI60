{-# OPTIONS_GHC -fdefer-type-errors #-}
-- Edgar Abundiz
-- lab2.hs
-- CSci 60, Lab 2
-- Combinatorics: In this lab, we are interested in finding *computational*
-- solutions to the kind of problems we looked at in class. So in addition to
-- working out mathematically how many possibilities there are, we'll be writing
-- code to *generate* all the possibilities, so that we can then count them
-- (using `length`) and check our work. In some cases, the possibilities may
-- be too difficult to count mathematically, in which case an algorithmic 
-- solution will have to suffice.
--
import Data.List (sort, sortBy, (\\))
import Data.Ord (comparing)

-- trace and traceShow are available for you to use in debugging.
import Debug.Trace (trace, traceShow)

--------------------------------------------------------------------------------
-- Problems
-- Note that for questions that ask "how many" possibilities there are, you
-- should write an *expression* (e.g., `length d6`) and not the actual value.
-- You should, of course, try to check the value Haskell gives you against 
-- the value the math says you should get. (But note that you should *not*
-- use the functions `fact`, `pm`, or `cm` here; those are for checking your
-- work, but the idea here is to actually *find* all the possible outcomes,
-- and then use the length to count them.)

-- How many ways are there to roll 4d6, if order is significant?
four_d6 = length (4 `d` 6)

-- How many distinct ways are there to roll 4d6, if order is not significant?
unorder_four_d6 = length $ distinct (map sort [[a,b,c,d]| a<-d6, b<-d6, c<-d6, d<-d6])

-- How many ways are there to roll 4d10, if we sum the rolls?
sum_four_d10 = length( length d4 `d` length d10) 

-- How many distinct ways are there to roll 4d10, summed?
sum_four_d10' = length $ distinct (map sort [[a + b + c + d]| a<-d10, b<-d10, c<-d10, d<-d10]) 

-- How many 5-card hands can be drawn from a standard 52-card deck, if 
-- order is significant?
five_card_hands = length $ perms deck 5

-- How many ways are there to rearrange the letters in the word POTATO,
-- not including indistinguishable rearrangements?
anagrams_potato = length (map sort [[a,b,c,d,e,f] | a <- ['P','O','T','A','T','O'], b <- ['O','T','A','T','O'], c <- ['T','A','T','O'], d <- ['A','T','O'], e <- ['T','O'],f <- ['O']]) `quot` 4

-- How many ways are there to roll 2d10 such that the first roll is less than
-- the second?
lt_2d10 = (length $ distinct $ map sort ( 2 `d` length d10)) - 10 -- -10 to get rid of the equal parings, probably better ways to do it but I am dont want to look at this lab anymore

-- Suppose we roll k `d` n. Of these, how many rolls will be sorted? You can
-- test for sorted-ness by writing a predicate that checks to see whether the
-- `sort` of a list is == to the original list.
sorted :: [Int] -> Bool
sorted x = x == sort x
sorted_rolls k n = countWith sorted [r | r <- k `d` n]

-- How many ways are there to roll 2d10 such that the first roll evenly 
-- divides the second? By "a evenly divides b" we mean b `rem` a == 0.
-- (I'm not sure if there's a nice mathematical way to count these; let me know
-- if you find one!)
divides_2d10 = count 0 [(a `rem` b) | a <- d10, b <- d10] 

-- Here's a die labeled with the sizes of the three smaller dice:
meta_die = [4,6,8]
-- Suppose we roll this die (= n) and a d6 (= k). We then roll k `d` n. 
-- How many outcomes are possible?
meta_meta_rolls =  length [k `d` n | k <- d6 , n <- meta_die, x <- k `d` n] 
-- As an optional exercise: try defining meta_die to include all the sizes of
-- dice. Can you get an answer before your computer explodes?

--------------------------------------------------------------------------------
-- Combinatorics "library"
--
-- These are the combinatorics functions available for you to use. You can also
-- use any of the functions in the Prelude that you need, as well as `sort`,
-- imported from Data.List, above. In particular, you may find `map` and
-- `filter` useful. 
--
-- Here's a quick reference to the definitions in this section:
--   distinct l              Sorts and removes duplicates from list l
--   k `d` n                 Rolls k n-sided dice
--   d4,d6,d8,d10,d12,d20    Gives the outcomes of a given die
--   Card                    Type for playing cards
--   deck                    52-element list of all cards 
--   perms l k               Generates all k-sized permutations of l
--   combine k l             Generalized (not just dice) version of `d`
--   countWith p l           Count outcomes in l that satisfy predicate p
--   count e l               Count outcomes that == e in l
--   countDistinct l         Pair each distinct element of l with its count
--   sortCounts ls           Sort the output of countDistinct on counts
--   p1 .&& p2               Predicate that is True if BOTH p1 and p2 are
--   p1 .|| p2               Predicate that is True if EITHER p1 or p2 is
--   p1 .>< p2               Predicate that is True if ONE of p1 or p2 is
--   (not . p)               Predicate that is True if p1 is False, and vice versa
--   fact n                  Gives the factorial of n
--   pm n k                  Gives the number of permutations of n, size k
--   cm n k                  Gives the number of combinations of n, size k

---------------------------------------
-- Dice and dice rolls

-- k `d` n
-- Roll k n-sided dice and collect all possible outcomes. This is the `d` 
-- function that we defined in class. 
d :: Int -> Int -> [[Int]]
0 `d` _  = [[]]
k `d` n  = concat [ map (i:) kdn' | i <- [1..n] ]
    where
        kdn' :: [[Int]]
        kdn' = (k - 1) `d` n

-- Predefined dice, built using `d`
d4 =  concat $ 1 `d` 4
d6 =  concat $ 1 `d` 6
d8 =  concat $ 1 `d` 8
d10 = concat $ 1 `d` 10
d12 = concat $ 1 `d` 12
d20 = concat $ 1 `d` 20

---------------------------------------
-- Deck of cards

data Suit = Spades | Diamonds | Clubs | Hearts
    deriving (Show, Eq, Ord)

data Card 
    = Face Suit Int
    deriving (Eq)

-- We make Card an instance of Show, so that cards can be printed nicely.
-- E.g., `Face Spades 4` will show up as "4 of Spades".
instance Show Card where
    show (Face s 1) = "Ace of " ++ (show s)
    show (Face s n) | n <= 10   = (show n) ++ " of " ++ (show s)
                    | n == 11  = "Jack of " ++ (show s)
                    | n == 12  = "Queen of " ++ (show s)
                    | n == 13  = "King of " ++ (show s)

-- We also make Card an instance of Ord, so that we can sort/distinct lists
-- of Cards.
instance Ord Card where
  a          <= a'           | a == a' = True
  (Face s n) <= (Face s' n') | s < s'  = True
                             | s == s' = n <= n'
  _          <= _                      = False

-- A full deck
deck :: [Card] 
deck = [Face Hearts i | i <- [1..13]] ++
       [Face Clubs i | i <- [1..13]] ++
       [Face Diamonds i | i <- [13,12..1]] ++
       [Face Spades i | i <- [13,12..1]]

------------------------------------
-- Combinatoric functions

-- distinct l
-- Given a list of (Ord-erable) items, `distinct` does two things:
-- 1. sorts the list
-- 2. removes duplicates
-- I.e., `distinct l` is a sorted, duplicate-free version of l. 
-- You can use this when we want to count the number of "distinct" possibilities.
distinct :: Ord a => [a] -> [a]
distinct = dedup . sort
    where
        dedup [] = []
        dedup [x] = [x]
        dedup (x1:x2:xs) | x1 == x2   = dedup (x2:xs)
                        | otherwise   = x1 : dedup (x2:xs)

-- perms l k
-- Generates all k-sized permutations of l.
perms :: Eq a => [a] -> Int -> [[a]]
perms _ 0 = [[]]
perms l r = [i:p | p <- pm', i <- l \\ p]
    where
        pm' = perms l (r-1)

-- combine k l
-- combine is the generalized version of `d`, that can work not just with
-- n-sided dice, but with any "base" collection of outcomes. For example
--   combine 4 d6
-- gives the same output as 
--   4 `d` 6
-- But you can use any list in place of `d6`.
combine :: Int -> [a] -> [[a]]
combine 0 _ = [[]]
combine k l = concat [ map (i:) kcn' | i <- l ]
    where
        kcn' = combine (k-1) l

-- countWith p l
-- Count the number of elements of l that satisfy the predicate p. For example,
--   countWith odd d20    
--   ===> 10
countWith :: (a -> Bool) -> [a] -> Int
countWith f l = length $ filter f l

-- count e l
-- Count the number of times e is present in l. E.g.,
--   count 10 $ map sum $ 4 `d` 6
-- will count the number of ways to get a 10, if we roll 4d6 and sum the dice.
count :: Eq a => a -> [a] -> Int
count e = countWith (==e)

-- countDistinct l
-- Like distinct, this will sort l and remove any duplicates. However, it will
-- pair up each element of l with number of duplicates of it that existed in
-- the original list. E.g.,
--   countDistinct [1,1,1,2,2,3]
--   ===> [(3,1),(2,2),(1,3)]
countDistinct :: Ord a => [a] -> [(Int,a)]
countDistinct l = [(count e l,e) | e <- distinct l]

-- sortCounts l
-- Ordinarily the output of countDistinct is sorted by the outcomes. sortCounts
-- re-sorts based on the counts for each outcome.
sortCounts :: Ord a => [(a,b)] -> [(a,b)]
sortCounts = sortBy (comparing fst)

-------------------------------------------------------------------------------
-- Predicate utilities
--
-- These functions make it (relatively) easy to combine predicates. E.g., 
-- suppose you are rolling 4d10 (summed) and you want to count all the outcomes
-- that are BOTH odd and >=10. While you could write your own predicate:
--   odd_ge10 x = odd x && x >= 10
--   ...
--   countWith odd_ge10 $ map sum $ 4 `d` 10
--   ===> ...
-- you can also just use the predicate combinators:
--   countWith (odd .&& (>=10)) $ map sum $ 4 `d` 10
--
-- Note that if you need to negate a predicate, you can use the built-in
-- `not` function like so:
--   countWith (not . odd) ...

-- Both
(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&) f1 f2 x = (f1 x) && (f2 x)

-- Either
(.||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||) f1 f2 x = (f1 x) && (f2 x)

-- Either, but not both
(.><) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.><) f1 f2 x = (e1 || e2) && not (e1 && e2)
    where
        e1 = f1 x
        e2 = f2 x

--------------------------------------------------------------------------------
-- Counting functions
-- As an aid to checking your work, these functions define the factorial,
-- P(n,k), and C(n,k).

fact :: Integer -> Integer
fact 0 = 1
fact n = product [1..n]

-- It's OK to use integer div-ision here, because we know that the result will
-- always be an integer.
pm :: Integer -> Integer -> Integer
pm n k = (fact n) `div` (fact (n-k))

cm :: Integer -> Integer -> Integer
cm n k = (fact n) `div` (fact k * fact (n-k))
