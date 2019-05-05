{-# LANGUAGE NoImplicitPrelude #-}

module Data.Nat.Internal (N(Z, S), (+), (*), (-), zero, one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve) where
-- Defines natural numbers and operations on them
-- TODO: Write div and mod

import qualified Prelude as Base
import Extra.Function
import Extra.Tuple

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

toInteger :: N -> Base.Integer
toInteger Z = 0
toInteger (S n) = 1 Base.+ toInteger n

fromInteger :: Base.Integer -> N
fromInteger n = case Base.compare 0 n of
    Base.GT -> fromInteger (n Base.- 1)
    Base.EQ -> Z
    Base.LT -> fromInteger (n Base.+ 1)

quotRem :: N -> N -> (N, N)
quotRem = mapAll fromInteger .> Base.quotRem <. toInteger

quot = fst . quotRem
rem = snd . quotRem

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