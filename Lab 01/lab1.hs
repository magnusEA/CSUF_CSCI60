{-# OPTIONS_GHC -fdefer-type-errors #-}
--Edgar Abundiz
-- lab1.hs
-- CSci 60, Lab 1
-- This lab is mostly about debugging: figuring out what to do when things go
-- terribly wrong. Unlike most labs, where your job is to fill in the 
-- (undefined) blanks, in this lab your job is to find and correct my mistakes!
--
import Data.List (sort, minimumBy, maximumBy)
import Data.Ord (comparing)
import Debug.Trace (trace, traceShow)

-------------------------------------------------------------------------------
-- Function reference
-- Here, for your information, is a list of all the built-in functions used in
-- the "wrong" definitions. 
--
--   tail l     -- Returns the tail of the list l. I.e., if l = (a:as), returns
--                 as.
--   a^b        -- Returns a to the b-th power. a^2 is a*a.
--   a `div` b  -- Divides a by b and rounds down to the nearest integer.
--              -- E.g., 5 `div` 2 == 2.
--   odd a      -- Returns True if a is odd (i.e., if a `rem` 2 == 1).
--   fromIntegral n -- Converts n from an Int(eger) to a Float or Double
--   

-- Let us assume that some CSci 60 class (not this one, of course) has had a
-- midterm, and the results are in. A sample of the students' grades are given
-- below, on a 0 to 1.0 scale, with the student ID given in the first element of
-- each pair, and the grade in the second.
type Grade = (Float,Float)

grades :: [Grade]
grades = [(102,0.8), (364,0.7), (237,0.2), (866,0.9), (527,0.5), (712,0.6), 
          (209,0.1), (595,0.4), (564,1), (418,0.5)]

-- We wish to perform some rudimentary statistical analyses on this data. 
-- Although Haskell has a lot of built-in functions that would make this
-- quite simple, we will write our own, in order to demonstrate both how things
-- can go wrong, and how to fix them when they do. (Note: the function
-- fromIntegral used below effectively converts an Int to a Float. Haskell does
-- not have implicit "widening" conversions of numeric types.)

-- len
-- First, we will need to know how many data points we are dealing with. The
-- function below finds (incorrectly) the length of a list. 
len :: [a] -> Int
len [] = 0 
len (a:as) = 1 + len as

-- getGrades
-- In order to perform our analyses on the grade data, we need to discard the
-- student ID numbers. This function does just that (or does it?).
getGrades :: [Grade] -> [Float]
getGrades [] = []
getGrades ((gr,id):gs) = id : getGrades gs

-- sumGrades
-- We'll want to find the average grade, and in order to do that we'll need the
-- sum of all the grades. 
sumGrades ::[Float] -> Float
sumGrades [] = 0
sumGrades (g:gs) = g + sumGrades (gs)

-- average
-- We'll want to find the average of all the grade data. There isn't anything
-- wrong with this definition, it's pretty hard to mess up. (fromIntegral, as
-- mentioned above, is needed to convert Int to Float.)
average :: [Grade] -> Float
average gs = (sumGrades (getGrades gs)) / (fromIntegral $ len gs)

-- lowHigh
-- What about the lowest and highest grades, and who got them? We want to 
-- find the largest and smallest grades, but return the pair (id,grade), not
-- just the grade.
lowHigh :: [Grade] -> (Grade,Grade)
lowHigh gs = (low gs, high gs)
  where
    low [(id,g)] = (id,g)
    low ((id,g):gs) = let (id',g') = low gs in
                        if g < g' then (id,g) else (id',g')

    high [(id,g)] = (id,g)
    high ((id,g):gs) = let (id',g') = high gs in
                        if g > g' then (id,g) else (id',g')

-- stddev
-- One thing we might want to know is how far from "average" each student is.
-- The standard deviation will tell us this. 
stddev :: [Grade] -> [Grade]
stddev gs = sqdevs gs
  where
    mean = average gs      -- Mean grade

    -- Squared deviations
    sqdevs [] = []
    sqdevs ((id,g):gs) = (id, dev g) : sqdevs gs

    dev g = (g - mean)^2   -- Squared deviation of a grade g

-- We can find the median by sorting the grades (on grade, not on id!) and then
-- choosing the ``middle'' value, or the mean of the two middle values.
median :: [Grade] -> Float
median gs = middle gs
  where
    grs = getGrades gs        -- Just grades 
    l = len grs               -- Length of gs
    mid = l `div` 2           -- Index of middle element

    -- If l is odd then l `div` 2 will be the middle element. If l is even
    -- then we average the element below and above the middle.
    middle gs = if odd l then 
                  grs !! mid
                else 
                  ((grs !! mid) + (grs !! (mid - 1 ))) / 2

-------------------------------------------------------------------------------
-- Give (correct!) values for the length, sum, average, etc. of the data set,
-- computed after you have corrected the above definitions.

grades_length :: Int
grades_length = 10

grades_sum :: Float
grades_sum = 5.7

mean_grade :: Float
mean_grade = 0.57

low_grade :: Grade
low_grade = (209.0, 0.1)

high_grade :: Grade
high_grade = (564.0, 1.0)

median_grade :: Float
median_grade = 0.55

-------------------------------------------------------------------------------
-- Here are the correct implementations of all of the above, in idomatic
-- Haskell. You can use these to check your work, and to see how the above
-- problems would be solved in "proper" Haskell.
--
-- NOTE: DO NOT just copy these definitions and replace the ones above with 
-- them; that's not the point of this exercise.

len' :: [a] -> Int
len' = length

getGrades' :: [Grade] -> [Float]
getGrades' = map snd

sumGrades' :: Num a => [a] -> a
sumGrades' = sum

average' :: [Grade] -> Float
average' gs = (sum $ map snd gs) / (fromIntegral $ length gs)

lowHigh' :: [Grade] -> (Grade,Grade)
lowHigh' gs = (minimumBy (comparing snd) gs, maximumBy (comparing snd) gs)

stddev' :: [Grade] -> [Grade]
stddev' gs = map (\(id,g) -> (id,(g - mean)^2)) gs
  where mean = average' gs

