{-# OPTIONS_GHC -fdefer-type-errors #-}
--
-- lab4.hs
-- Finishing up probability
-- This lab includes the "extended" combinatorics and probability library.
-- For this lab I'm trying to go less in the direction of things you could 
-- figure out mathematically, and more towards things that pretty much require
-- a computer to work out. 
--
import Data.List
import Debug.Trace
import Data.Ord (comparing)


data CompassDir = N | E | S | W deriving (Eq, Ord, Show)
type Coord = (Int,Int)

-- Consider the city streets example I gave in class. We have a 5x5 city 
-- block region, ranging from (0,0) to (5,5) (here I will use Cartesian 
-- coordinates rather than street names, and I've limited the size to make the 
-- problems tractable). First, in order to move around, we will need functions 
-- to manipulate coordinates. Assuming that N is positive y, and E is positive 
-- x, complete the definition of the functionn `move`, which takes a coordinate 
-- pair and moves in the direction of the specified compass direction. E.g.,
--   move N (1,2)
--   ===> (1,3)
move :: CompassDir -> Coord -> Coord
move N (x,y) = (x , y + 1)
move E (x,y) = (x + 1 , y)
move S (x,y) = (x , y - 1)
move W (x,y) = (x - 1 , y)

-- A "path" is just a list of directions, e.g., [N,N,E,S,E]. 
type Path = [CompassDir]
-- Suppose we start at some point and follow a path through the streets, where
-- a path is a list of compass directions. Complete the definition of `follow`
-- which starts at a given point, follows a path, and returns the list of 
-- points along it. E.g.,
--   follow (0,0) [N,N,E,E] 
--   ===> [(0,0), (0,1), (0,2), (1,2), (2,2)]
follow :: Coord -> Path -> [Coord]
follow (x,y) [] = [(x,y)]
follow (x,y) (d:ds) =(x,y) : follow (move d (x,y)) (ds)

-- Does a list of coordinates "intersect" (contain) a given point? That's what
-- `intersects` aims to tell us. Complete its definition.
intersects :: Coord -> [Coord] -> Bool
intersects (x,y) ps = ((x ,y) `elem`) ps 

-- all_paths n  (Given)
-- `all_paths n` gives us the list of all paths of length n
all_paths :: Int -> [Path]
all_paths = some_paths [N,S,E,W]

-- some_paths dirs n  (Given)
-- some_paths gives all the paths of length n, that only use the directions
-- in dirs. E.g., 
--   some_paths [N,E] 10
-- will give the paths of length 10 composed only of N and E directions.
some_paths :: [CompassDir] -> Int -> [Path]
some_paths _  0 = [[]]
some_paths ds n = concat [map (d:) some_paths' | d <- ds]
  where
    some_paths' = some_paths ds (n-1)

---------------------------------------
-- OK, here's where the problems start:

-- Consider all the paths of length 10, using all compass directions. What
-- is the probability that a random path will go from (0,0) to (5,5)?
diagonal :: [Coord] -> Bool
diagonal [] = False
diagonal (a:as) = a == (5,5) || diagonal as
goes_diagonal = prob (diagonal) [ follow (0,0) p | p <- all_paths 10]

-- Again considering all the paths of length 10, what is the probability that
-- a path will intersect itself? (A path intersects itself if its `follow` 
-- contains duplicates.) Note that "distinct" will do the wrong thing here, 
-- because it will sort the path, rearranging the points. But you can use the
-- function `nub` to remove duplicates from a path without sorting it.
cross :: Path -> Bool
cross a = follow (0,0) a /= nub (follow (0,0) a) 
self_intersects = prob (cross) (all_paths 10)

-- Consider all the paths of length 10, that go only N or E. What is the 
-- probability that a random path will go from (0,0) to (5,5)?
ne_goes_diagonal =  prob (diagonal) [ follow (0,0) p | p <- some_paths [N,E] 10]


-- Consider the paths from the previous problem (length = 10, using only N or E).
-- What is the probability that a random path will cross the diagonal (i.e., 
-- that for any point on the path (x,y), y > x)? You may find it helpful to
-- define a helper function that tells you when a given point (x,y) is above
-- the diagonal.
cordcheck :: Coord -> Bool
cordcheck (x,y) = (y>x)
overdiagonal :: [Coord] -> Bool
overdiagonal [] = False
overdiagonal (d:ds) = cordcheck d || overdiagonal ds
ne_crosses_diagonal = prob (overdiagonal) [ follow (0,0) p | p <- some_paths [N,E] 10]

---------------------------------------
-- The Monty Hall problem
-- We're going to build a simulation of the Monty Hall problem. We'll use
-- lists to represent the state of the doors (0 = empty, 1 = prize), so here
-- are all the possible configurations of doors:
doors :: [[Int]]
doors = [[1,0,0], [0,1,0], [0,0,1]]

-- For each door-configuration, there are three possible player-choices. Our 
-- goal here is to pair each door with all three possible choices. We will
-- represent the combination of a door-config and a choice as a pair:
-- (choice, [doors...]). Complete the definition of the possible choices.
-- (Note that the resulting list should have 9 entries.)
choices :: [(Int,[Int])]
choices =  [(x,doors) | x<-[0,1,2] , doors <- [[1,0,0], [0,1,0], [0,0,1]]]

-- other x y
-- `other` is a helper function for figuring out which door is the "other" door.
-- Given x and y, both 0 <= x,y < 3, with x /= y, `other x y` will return the
-- other value in that range that is not equal to either x or y.
other :: Int -> Int -> Int
other x y | x < 0 || y < 0 || x >= 3 || y >= 3 || x == y    = 
    error "Invalid arguments to other."
other 0 y = (1 - y) + 2           -- y is either 1 or 2
other 1 y = (1 - (y `div` 2)) * 2 -- y is either 0 or 2
other 2 y = 1 - y                 -- y is either 0 or 1

-- indexOf a l
-- If you need to find which door is the winning door, you can use the built-in
-- function indexOf, which returns the index of a in the list l.
indexOf :: Eq a => a -> [a] -> Int
indexOf a (a':as) | a == a'   = 0
                  | otherwise = 1 + indexOf a as

-- Now comes the tricky part: given an arrangement of doors and the player's
-- choice, the host's choice is almost determined, *except* in the case where
-- the player chose correctly. We'll write a function that will give us the
-- possible host choice(s), for each player choice+door. The idea is that
-- given 
--   (player choice, [doors]) 
-- we will return
--   [(player choice, host choice, [doors]), 
--    (player choice, host choice, [doors])]
-- where the player choice and the doors are the same, but the host's choice
-- is whatever it was forced to be. Note that the list has two elements, both 
-- identical; this is so that the probabilities will work out correctly later.
-- In the case where the host has two choices, we'll return a list with *two* 
-- *different* elements:
--   [(player choice, host choice 1, [doors]),
--    (player choice, host choice 2, [doors])]
da_winner :: Int -> [Int]
da_winner x = if x == 0 then [1,2] else if x == 1 then [0,2] else [0,1]

host_chooses :: (Int,[Int]) -> [(Int,Int,[Int])]
host_chooses (pc, doors) = if pc /= (indexOf 1 doors) then [(pc, hc, doors) | hc <- [(other pc (indexOf 1 doors)), (other pc (indexOf 1 doors))]] else [(pc, hc, doors) | hc <- da_winner pc] 

-- Now, for every possible choice in `choices`, we apply host_chooses to it
-- and stick all the results together to get a big list of all the possible ways
-- things could work out, for each doors arrangement, player choice (pc), and 
-- host choice (hc).  
--                    (pc, hc, doors)
all_possibilities :: [(Int,Int,[Int])]
all_possibilities = concat [(host_chooses x) | x <- choices ]

-- Given all the possibilities, what is the probability that the player's 
-- first choice is correct (will result in a win)?
one_and_done :: (Int,Int,[Int]) -> Bool
one_and_done (pc,hc, doors) = pc == (indexOf 1 doors)
player_stay_prob = prob (one_and_done) all_possibilities

-- What is the probability that the "switch" choice would be correct?
try_again :: (Int,Int,[Int]) -> Bool
try_again(pc,hc, doors) = pc /= (indexOf 1 doors)
player_switch_prob = prob (try_again) all_possibilities



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
d :: (Eq a, Num a, Enum b, Num b) => a -> b -> [[b]]
0 `d` _  = [[]]
k `d` n  = concat [ map (i:) kdn' | i <- [1..n] ]
    where
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
