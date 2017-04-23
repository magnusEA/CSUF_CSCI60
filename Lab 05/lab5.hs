{-# OPTIONS_GHC -fdefer-type-errors #-}
--Edgar Abundiz
-- lab5.hs
-- Natural numbers, inductive and recursive definitions
--

-- Here is the data type defining natural numbers from Z(ero) and S(uccessor)
data Nat = Z | S Nat
    deriving (Show)

-- Here are two helper functions that should make it easier for you to work
-- with Nats. They allow you to convert Ints to/from Nats, so you don't have
-- to sit there counting S's to figure out what number a Nat corresponds to.
toInt :: Nat -> Int
toInt Z = 0
toInt (S n) = 1 + toInt n

toNat :: Int -> Nat
toNat n | n < 0 = error "Nats do not support negative numbers!"
toNat 0 = Z
toNat n = S (toNat (n-1))


-- Once you have defined eq and neq below, this will allow you to use == and
-- /= on Nats as you would expect.
instance Eq Nat where
  (==) = eq
  (/=) = neq

-- Likewise, after you have defined `lt` and `gte` this will allow you to use
-- all the normal comparison operators <, >=, <=, etc.
instance Ord Nat where
  compare a b | a `lt` b  = LT
              | a `eq` b  = EQ
              | otherwise = GT

infixl 7 `times`
infixl 7 `divBy`
infixl 7 `remd`
infixl 6 `plus`
infixl 6 `minus`
infix 4 `lt`
infix 4 `gte`
infix 4 `eq`
infix 4 `neq`

--------------------------------------------------------------------------------
-- Problem set

-- Write the recursive definitions of <, >=, +, *, -, and / as the functions
-- below. Note that I have setup the fixity for these so that you can use them
-- as infix operators. E.g.
--
--   a `plus` b `times` c
--
-- will parse as 
-- 
--   a `plus` (b `times` c)
--
-- as you would expect.

-- I have highlighed for you which argument the functions will be recursive on,
-- by expanding out the cases for that argument. You may still have to edit the
-- other argument to the functions.


{-
Less than is given for you, as an example.
LT-Zero------------------------
              Z < (S n)

                a < b                   
LT-Succ------------------------
            (S a) < (S b)
-}
lt :: Nat -> Nat -> Bool
lt Z     (S b) = True
lt (S a) (S b) = lt a b
lt _      _    = False
{- Greater-than-or-equal-to

GE-Zero------------------------
               n >= Z

                a >= b                   
GE-Succ------------------------
            (S a) >= (S b)
-}
gte :: Nat -> Nat -> Bool
gte a     Z = True
gte (S a) (S b) = gte a b
gte _     _ = False
{-
Addition is given for you, as an example.
+-Zero------------------------
             Z + n = n          

             a + b = c                   
+-Succ------------------------
          (S a) + b = S c
-}
plus :: Nat -> Nat -> Nat
plus Z     b = b
plus (S a) b = (S c)
  where
    c = plus a b

{- Multiplication

*-Zero------------------------
             Z * n = Z          

             a * b = c                   
*-Succ------------------------
          (S a) * b = b + c
-}
times :: Nat -> Nat -> Nat
times Z     b = Z
times (S a) b = plus b c 
	where
		c = times a b

{- Subtraction

--Zero------------------------
             n - Z = n          

               a - b = c                   
--Succ------------------------
            (S a) - (S b) = c
-}
minus :: Nat -> Nat -> Nat
minus a Z     = a
minus Z b     = error "No Negatives"
minus (S a) (S b) = c
          where
          c = minus a b

{- Monus
                 a < b
Mon-Zero------------------------
              a mon b = 0

              a mon b = c
Mon-Succ------------------------
           S(a) mon S(b) = c
-}
predator :: Nat -> Nat
predator Z = Z
predator (S a) = a

monus :: Nat -> Nat -> Nat
monus a     Z = a
monus a b | a `lt` b = Z
monus a b | a `gte` b  = a `minus` b
monus (S a) (S b) = predator x
        where 
        x = monus a b
{- Division
                a < b
/-Zero------------------------
              a / b = Z          

        a >= b    a - b = a'  a' / b = c                                     
/-Sub-------------------------------------
                 a / b = S c                     
-}
divBy :: Nat -> Nat -> Nat
divBy a Z = error "No dividing by 0"
divBy Z a = Z
divBy a b | a `lt` b      = Z -- base case
          | otherwise  = (S c) -- recursive case
          where 
          a' = minus a b 
          c = a' `divBy` b
{- Remainder
                a < b
Rem-Zero-----------------------
             a rem b = a
 
        a >= b    a - b = a'    a' rem b = c
Rem-Sub---------------------------------------
                 a rem b = c
-}
remd :: Nat -> Nat -> Nat
remd a b | a `lt` b      = a -- base case
         | otherwise     = (S c)		 -- recursive case
		where
		a' = minus a b
		c = a' `divBy` b

{- Divides 

Div-Zero-----------------------
                a | 0

        b > 0   b - a = b'   a | b'
Div-Sub------------------------------
                a | b
-}
divides :: Nat -> Nat -> Bool
divides a b | b `lt` a = False
divides a Z       = True
divides a b@(S _) = True


-- Fill in the inference rule definitions for equality and inequality, and
-- then use them to write recursive functions defining those relations.
{-
                                   
Eq-Zero----------------------------
                 Z = Z                   

				 
                a == b                   
Eq-Succ---------------------------- 
            (S a) == (S b)                 


                a > b                   
NE-Zero1---------------------------
				a /= Z

				
                b > Z                   
NE-Zero2---------------------------
                Z /= b                   

				
               a /= b                    
NE-Succ----------------------------
			(S a) /= (S b)

-}
eq :: Nat -> Nat -> Bool
eq Z Z = True
eq a Z = False
eq Z _ = False
eq (S a) (S b) = eq a b


neq :: Nat -> Nat -> Bool
neq Z Z = False
neq (S a) Z = False
neq Z (S b) = True
neq (S a) (S b) = neq a b