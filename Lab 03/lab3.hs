{-# OPTIONS_GHC -fdefer-type-errors #-}
-- Edgar Abundiz
-- lab3.hs
-- More combinatorics, and some basic probability.
-- This lab includes the "extended" combinatorics and probability library.
--
import Data.List
import Debug.Trace
import Data.Ord (comparing)

--------------------------------------------------------------------------------
-- Lab problems

-- Combinatorics problems

-- Rolling 4d10, what is the number of rolls in which at least one 6 appears?
four_d10_has_6 = count True [if a == 6 then True
							else if	b == 6 then True
							else if	c == 6 then True
							else if	d == 6 then True
							else False | a <- d10, b <- d10, c <- d10, d <- d10]

-- How many unique permutations are there of the letters in the word
-- "BANANA"?
perms_banana = length $ distinct $ permutations "BANANA" 

-- Consider all the *odd* numbers between 1 and 1000. How many are *not* 
-- divisible by 3, 7, 11, or 13?
not_div_by_371113 = length $ [x | x <- [1..1000],
								not (even x
								|| x `rem` 3 == 0
								|| x `rem` 7 == 0
								|| x `rem` 11 == 0
								|| x `rem` 13 == 0)]

-- How many derangements are there of the word "TEACHING"?
teaching_derangements = length $ derangements "TEACHING"


--------------------------- Probability problems ------------------------------

-- What is the probibility, when rolling 3d12, that the three values will be
-- in strictly descending order (e.g., 3,2,1 but not 3,3,2 or 1,2,3)
descending_3d12 = prob (==True) [a > b && b > c | a <- d12, b <- d12, c <- d12]

-- Assume that we *only* roll descending 3d12 (i.e., non-descending rolls are
-- ignored). 1) What is the probability that a random roll will contain a
-- 12? 2) What about all 3d12 rolls, not just the descending ones: what is 
-- the probability of a roll containing a 12? Are these probabilities different?
desc_3d12_has_12 = prob (12 `elem`) [k | k <- 3`d` 12, k!!0 > k!!1, k!!1 > k!!2 ]
norm_3d12_has_12 = prob (12 `elem`) (3 `d` 12)

-- Consider the unique permutations of "BANANA". What is the probability that
-- a random permutation will start and end with an "A"? (Note that the function 
-- `head l` will give you the first element of l, while `last l` will give you 
-- the last.) You will want to write the helper function `startsEndsWithA`
-- which returns True for this condition:
startsEndsWithA :: String -> Bool
startsEndsWithA s = head s == 'A' && last s == 'A' 
banana_as = prob (startsEndsWithA) (distinct(perms "BANANA" 6))

-- This function returns True if `as` is a derangement of `bs`, assuming they
-- both have the same items to start with. 
is_derangement as bs = and $ zipWith (/=) as bs
-- Consider all the permutations of [1..10]. What is the probability that a
-- random permutation will be a derangement?
perm_is_derange = prob (is_derangement [1..10]) $permutations [1..10] 
-- Consider all the derangements of the word "TEACHING". What is the probability
-- that a random derangement will have "CH" as a substring? 
-- (You can use `isInfixOf` to test whether "CH" is a substring.)
teaching_drgs_ch = prob ("CH" `isInfixOf`) $ derangements "TEACHING" 



--------------------------------------------------------------------------------
-- Combinatorics and probability "library"
--
-- These are the combinatorics functions available for you to use. You can also
-- use any of the functions in the Prelude that you need, as well as `sort`,
-- imported from Data.List, above. In particular, you may find `map` and
-- `filter` useful. 
--
-- Here's a quick reference to the definitions in this section:
--
--  Combinatoric functions:
--   distinct l              Sorts and removes duplicates from list l
--   k `d` n                 Rolls k n-sided dice
--   d4,d6,d8,d10,d12,d20    Gives the outcomes of a given die
--   Card                    Type for playing cards
--   deck                    52-element list of all cards (incl. jokers)
--   perms l k               Generates all k-sized permutations of l
--   derangements l          Generates all derangements of l
--   combine k l             Generalized (not just dice) version of `d`
--   countWith p l           Count outcomes in l that satisfy predicate p
--   count e l               Count outcomes that == e in l
--   countDistinct l         Pair each distinct element of l with its count
--   sortCounts ls           Sort the output of countDistinct on counts
--
--  Predicate combinators:
--   p1 .&& p2               Predicate that is True if BOTH p1 and p2 are
--   p1 .|| p2               Predicate that is True if EITHER p1 or p2 is
--   p1 .>< p2               Predicate that is True if ONE of p1 or p2 is
--   (not . p)               Predicate that is True if p1 is False, and vice versa
--
--  Probability functions:
--   prob f l                Calculates the probability of event f occuring in
--                           the list of outcomes l
--   when e f l              Calculates the conditional probability of f
--                           occuring in l, *assuming* that e has occured
--   average l               Given a list of numeric outcomes, calculates the
--                           average outcome.
--
--  Numeric functions:
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
    = RedJoker
    | BlackJoker
    | Face Suit Int
    deriving (Eq)

-- We make Card an instance of Show, so that cards can be printed nicely.
-- E.g., `Face Spades 4` will show up as "4 of Spades".
instance Show Card where
    show RedJoker = "Red Joker"
    show BlackJoker = "Black Joker"
    show (Face s 1) = "Ace of " ++ (show s)
    show (Face s n) | n <= 10   = (show n) ++ " of " ++ (show s)
                    | n == 11  = "Jack of " ++ (show s)
                    | n == 12  = "Queen of " ++ (show s)
                    | n == 13  = "King of " ++ (show s)

-- We also make Card an instance of Ord, so that we can sort/distinct lists
-- of Cards.
instance Ord Card where
  a          <= a'           | a == a' = True 
  RedJoker   <= _                      = True
  BlackJoker <= (Face _ _)             = True
  (Face s n) <= (Face s' n') | s < s'  = True
                             | s == s' = n <= n'
  _          <= _                      = False

-- A full deck
deck :: [Card] 
deck = [RedJoker, BlackJoker] ++ 
       [Face Hearts i | i <- [1..12]] ++
       [Face Clubs i | i <- [1..12]] ++
       [Face Diamonds i | i <- [12,11..1]] ++
       [Face Spades i | i <- [12,11..1]]

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

-- derangements l
-- Gives all the derangemenets of the list l. Note that for speed we are
-- using the built-in `permutations` function. 
derangements :: Eq a => [a] -> [[a]]
derangements l = filter (and . zipWith (/=) l) $ permutations l

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
(.||) f1 f2 x = (f1 x) || (f2 x)

-- Either, but not both
(.><) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.><) f1 f2 x = (e1 || e2) && not (e1 && e2)
    where
        e1 = f1 x
        e2 = f2 x

--------------------------------------------------------------------------------
-- Probability functions

-- prob
-- prob will tell us the probability of event f occuring in the set of outcomes
-- l. For example, we could do
--
--   prob odd $ map sum $ 4 `d` 6
--
-- I.e., what is the probability of rolling an odd total, when we roll 4d6?
prob :: Eq a => (a -> Bool) -> [a] -> Float
prob f l = (fromIntegral $ countWith f l) / (fromIntegral $ length l)

-- when e f l
-- Conditional probability: what is the probability of f *given* e, in l?
when :: Eq a => (a -> Bool) -> (a -> Bool) -> [a] -> Float
when e f l = prob f $ filter e l

-- Average outcome (for numeric outcomes). This is just the average of the
-- outcomes.
average :: Fractional a => [a] -> a
average l = (sum l) / (fromIntegral $ length l) 

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
