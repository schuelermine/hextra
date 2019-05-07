{-# LANGUAGE NoImplicitPrelude #-}

module Data.Nat (N(Z, S), (+), (*), (-), toInteger, fromInteger, quotRem, quot, rem, subtract, zero, one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve) where
-- Defines natural numbers and operations on them

import qualified Prelude as P
import Extra.Function
import Extra.Tuple
import Extra.Num

data N = Z | S N
-- Inductive natural number type
-- Z is 0, S is the successor function
-- S is equivalent to (+ 1)

(+) :: N -> N -> N
Z     + y = y
(S x) + y = S (x + y)
-- Addition of natural numbers
-- Zero is the identity, and therefore, a base case
-- Otherwise recursively peel away S layers from argument, apply (+) again, and apply S on the whole

(*) :: N -> N -> N
Z     * _ = Z
(S x) * y = (x * y) + y
-- Multiplication of natural numbers
-- Explanation 1:
--  Zero is an annihilator, and therefore, a base case
--  Otherwise recursively peel away S layers from one argument,
--  apply (*) again, and apply (+) with the untouched argument on the whole
--  annihilator = element which is always returned by the operation (in this case (*))
-- This works because multiplication is iterated addition
-- Explanation 2:
--  This is an implementation of iterated addition
--  Whenver one is removed from argument 1, argument 2 is added to the actual value,
--  so argument 2 is added argument 1 times to the actual value,
--  and since the base case for the recursion is always zero, the result is argument 1 times argument 2

(-) :: N -> N -> N
Z     - y     = y
(S x) - (S y) = x - y
-- Difference between two natural numbers
-- This exploit the fact that,
-- if you add something to two numbers, the difference stays the same
-- Peels away a layer of S on both arguments and then apllies itself again

min :: N -> N -> N
min Z _         = Z
min _ Z         = Z
min (S x) (S y) = S (min x y)
-- Finding the smallest of two natural numbers.
-- Zero is smaller than any other natural number, this is the recursion base case
-- When subtracting one, you don't change which number is the smallest,
-- so you just need to add one.

max :: N -> N -> N
max Z y         = y
max x Z         = x
max (S x) (S y) = S (max x y)
-- Finding the largest of two natural numbers.
-- Any other natural number is larger than zero, this is the recursion base case
-- When subtracting one, you don't change which number is the largest,
-- so you just need to add one.

toInteger :: N -> P.Integer
toInteger Z     = 0
toInteger (S n) = 1 P.+ toInteger n
-- Converts natural numbers to integers
-- Z converts to zero, and S converts to +1

fromInteger :: P.Integer -> N
fromInteger n = case P.compare 0 n of
    P.GT -> S (fromInteger (n P.+ 1))
    P.EQ -> Z
    P.LT -> S (fromInteger (n P.- 1))
-- Converts integers to natural numbers
-- Reduces value towards 0 while applying S
-- Negative integers don't raise errors, but are treated like their positive counterparts.

quotRem :: N -> N -> (N, N)
quotRem = mapAll fromInteger .> P.quotRem <. toInteger
-- Division and Modulo of natural numbers
-- This just converts them to integers, divides, and converts back
-- (<.) and (.>) are functions for composing two-argument and one-argument functions

quot = P.fst .> quotRem
rem = P.snd .> quotRem
-- Division and Modulo of natural numbers, but individually
-- Just extracts the first and second elemts of the result of quotRem

subtract :: P.Integral n => N -> N -> n
subtract = (P.-) <. toIntegral
-- Subtraction of two natural numbers. The result is not necessarily a natural number
-- For example, 7 - 21 = (-14), which isn't a natural number,
-- hence a type including an integral

-- Named numbers (up to 12):

zero = Z

one = S Z -- One (Zero + 1)

two = S one

three = S two

four = S three

five = S four

six = S five

seven = S six

eight = S seven

nine = S eight

ten = S nine

eleven = S ten

twelve = S eleven

-- Instances of N:

instance Num N where
    (+) = (+)
    (-) = (-)
    (*) = (*)
    fromInteger = fromInteger
    negate = P.id
    abs = P.id
    signum = P.const one

instance Eq N where
    Z     == Z     = P.True
    Z     == _     = P.False
    _     == Z     = P.False
    (S x) == (S y) = x P.== y

instance Ord N where
    compare  Z     Z    = P.EQ
    compare  Z     _    = P.LT
    compare  _     Z    = P.GT
    compare (S x) (S y) = P.compare x y

instance Enum N where
    succ = S
    pred Z = Z
    pred (S x) = x
    fromEnum = P.fromInteger P.. toInteger
    toEnum = P.fromIntegral

instance Real N where
    toRational = P.toRational P.. toInteger

instance Integral N where
    quotRem = quotRem
    divMod = quotRem
    toInteger = toInteger

instance Show N where
    showsPrec x = P.showsPrec x P.. toInteger
    show = P.show . toInteger
    showList = P.showList P.. map toInteger