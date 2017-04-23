
-- Edgar Abundiz
-- lab8.hs
-- CSci 60, lab 8, programming component
--

{------------------------Inference Rules in Haskell-----------------------------

I mentioned in lecture that we can think of inference rules as being kind of 
like "functions" which take their premises as arguments, and return their
conclusion as a result. Although it's kind of silly, we're going to take a look
at what that would actually look like. 

If the rules themselves are functions, then what are the judgments? Judgments 
are in fact *types*, and the act of "checking a derivation" (i.e., making sure
that all the premises come from conclusions correctly) is exactly the same
as the process Haskell uses when it checks the types of your code. We can use
this to encode both inference rules, and derivations, into Haskell types.

Here, we'll define new types corresponding to all the judgments in the rules
from the paper portion of this lab:
-}
data Wednesday
data TeachingCsci60
data ItsCold
data HaveOfficeHours
data ItsRaining
data HaveARainJacket
data StudentsBored
data SleptIn
data ForgotLunch
data Hungry
data HereLate
data BroughtTea

-- This "special" type is used to make your definitions (below) not compile
-- until they are filled in with the correct types.
data BadJudgment = BadJudgment

-- Note that none of these types have any values associated with them! This is
-- OK, because Haskell will let us use `undefined` as a value of any type. This
-- is the one time where you don't need to replace `undefined` with something;
-- it's supposed to be there. Here are all the axiomatic rules:
r1 :: Wednesday
r1 = undefined

r2 :: TeachingCsci60
r2 = undefined

r3 :: ItsCold
r3 = undefined

-- We define the other rules as functions, taking the premises' types as 
-- arguments, and returning the conclusion's type as a result.
r4 :: Wednesday -> TeachingCsci60 -> HaveOfficeHours
r4 = undefined

r5 :: ItsRaining -> ItsCold -> HaveARainJacket
r5 = undefined

r6 :: TeachingCsci60 -> StudentsBored
r6 = undefined

r7 :: SleptIn -> ForgotLunch
r7 = undefined

r8 :: HaveOfficeHours -> ForgotLunch -> Hungry
r8 = undefined

r9 :: HaveOfficeHours -> Wednesday -> HereLate
r9 = undefined

r10 :: TeachingCsci60 -> ItsCold -> BroughtTea
r10 = undefined
    
{- We can now use Haskell to "check" our derivations. For example, here is the
   derivation

    --------------------r2
    I'm Teaching CSci 60
   -----------------------r6
    My Students are Bored

   in functional form:
-}
proof_bored_students :: StudentsBored
proof_bored_students = r6 r2

--------------------------------------------------------------------------------
-- Problems:
-- Construct functional derivations of all of the following conclusions:
-- (Note that you should replace these `BadJudgment`s with your definitions.)

proof_wednesday :: Wednesday
proof_wednesday = r1

proof_teaching_60 :: TeachingCsci60
proof_teaching_60 = r2

proof_cold :: ItsCold
proof_cold = r3

proof_office_hours :: HaveOfficeHours
proof_office_hours = r4 r1 r2

proof_here_late :: HereLate
proof_here_late = r9 (r4 r1 r2) (r1)

proof_brought_tea :: BroughtTea
proof_brought_tea = r10 r2 r3

-- How can we handle assumptions? E.g., consider rule 5; it relies on the
-- premise "It's raining" but this is not the conclusion of any other rule. 
-- So we cannot conclude that "I'll have a rain-jacket" normally, but if we
-- *assume* that it's raining, hypothetically, then we should be able to.
-- Put in functional terms, the problem is that the type ItsRaining has no
-- value associated with it. To assume it, we need to temporarily give ourselves
-- a value of type ItsRaining, so that we can pass it to rule 5 and get its
-- conclusion. So we'll use let..in:
proof_have_rainjacket :: HaveARainJacket
proof_have_rainjacket = let raining :: ItsRaining
                            raining = undefined
                        in
                            r5 raining r3

-- Use this trick to construct the hypothetical proof that "I'm Hungry", 
-- assuming "I slept in":
proof_hungry :: Hungry
proof_hungry = let sleep :: SleptIn
                   sleep = undefined
                   
                in
                   r8 (r4 r1 r2) (r7 sleep)