{-# LANGUAGE TypeOperators #-}
--
-- lab9.hs
-- CSci 60, lab 9, programming component
--

data BadJudgment = BadJudgment

{-
This portion of the lab consists entirely of implementing the derivations from
the paper part of the lab in Haskell. In order to do this, we'll need (as in lab
8) types corresonding to our propositions A, B and C:
-}
data A
data B
data C

{-
If we want to work with logical /\ and \/, we will also need types corresponding
to these *compound* propositions. Fortunately, Haskell has some types built-in
that will serve this purpose, although we'll rename them to make them more 
familiar. For A /\ B, we will write the type A /\ B, and
for A OR B, we will write A \/ B. 
-}
type (/\) = (,)
type (\/) = Either
{-
A proof (derivation) of A /\ B is just a pair of proofs. That is, if x is a 
proof of A, and y is a proof of B, then the value (x,y) is a proof of A /\ B.

A proof (derivation) of A \/ B has to correspond to either the Or-Right1
or Or-Right2 rule. That is, it has to tell us whether we are supplying a proof
of A, or a proof of B. We do this with Left and Right:

    Left x   ==> Proof of A \/ B, by way of proof of A
    Right y  ==> Proof of A \/ B, by way of proof of B
-}

{-
What about implication? What type corresponds to A -> B? Well, it's the type of 
functions that take an A, and return a B! In fact, the built-in -> type that
we've been using all along will serve perfectly well as a functional analogue to
logical implication. Notice what happens when we construct a function of type
A -> B:

  * The function gets an argument of type A. This is equvalent to "assuming" that
    we have an A. We don't get an *actual* A until the function is called, but
    we can pretend that we have one.

  * Assuming we have an A, we can look at it and take it apart 
    (via pattern-matching) and do whatever we need to do in order to transform
    it into a B. This is exactly what we do with the assumptions: we take them
    apart according to the left-rules and use their components to (re)construct
    the conclusion.

Since this is all very abstract, here's a concrete example. We want to construct
the functional derivation that proves

    (A \/ B) -> (B \/ A)

That proposition is in fact the type of our proof value. Note that because it
has a -> as its top-level type, we will be writing a function, rather than a
plain value.
-}
commutative_or :: (A \/ B) -> (B \/ A)
{-
Because the function has (A \/ B) as its input type, that means it could get
either a Left x or a Right y (with x a proof of A, or y a proof of B) as an
actual argument. We use pattern matching to essentially apply the Or-Left rule
and handle these two cases:

    commutative_or (Left a) = ... 
    commutative_or (Right b) = ...

In each case, we have to construct the "opposite" version of the input. So if
we got a proof of A via Left, we want to return it as a Right, and vice versa.
This leads us to:
-}
commutative_or (Left a) = Right a
commutative_or (Right b) = Left b

--------------------------------------------------------------------------------
-- Problems

{-
Each of these problems corresponds exactly to one of the written problems. 
Remember that if you have an \/ as an assumption, that usually means you'll need
to pattern-match in the arguments on Left vs. Right. Similarly, if you have
/\ as an assumption, you'll need to pattern match on (a,b).
-}

-- If I give you an A and B, you can give me back one or the other
-- (note that there are two valid solutions of this, depending on whether you
-- choose to give me an A or a B). I've added the pattern match to the arguments
-- for you, to extract the proofs of A and B from the /\ input.
and_to_or :: (A /\ B) -> (A \/ B)
and_to_or (a,b) = BadJudgment

-- If I give you an A and a B, you can give me back a B and an A. (You'll have
-- to supply the correct argument pattern here.)
commutative_and :: (A /\ B) -> (B /\ A)
commutative_and (a,b) = BadJudgment


-- If I give you an A or a B, *and* an A or a C, then you can give me back an 
-- A, or a B and a C. Hint: there are *four* argument patterns you'll need to
-- match against; I've given you the first one. Rather than use x,y, etc., I
-- suggest using a,b,c to remind yourself of what kind of thing(s) you have.
undistribute_and :: ((A \/ B) /\ (A \/ C)) -> (A \/ (B /\ C))
undistribute_and ((Left a1),  (Left a2))  = BadJudgment
undistribute_and _ = BadJudgment
undistribute_and _ = BadJudgment
undistribute_and _ = BadJudgment

-- If I give you an A, and a machine that converts A's into B's, you can give me
-- an A and a B. Note that the argument a_to_b is actually a *function*! You can 
-- pass it something of type A, and it will give you something of type B.
ab_to_both :: (A /\ (A -> B)) -> (A /\ B)
ab_to_both (a, a_to_b) = BadJudgment