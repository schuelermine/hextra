{-# LANGUAGE ExplicitForAll #-}

module Data.Cirq.Utils where
-- Exports predefined Cirqs.

import Data.Cirq.Base as Cq
{-# LANGUAGE ExplicitForAll #-}

import Control.Arrow as Arr

cqTotal :: forall a. Num a => Cirq a a
cqTotal = cqAccum 0 (+)
-- Keeps track of the total of numbers passed to it.
-- When a number is passed to it, the replacement function will add that number to its number,
-- and so on.

cqTotalM :: forall a. Monoid a => Cirq a a
cqTotalM = cqAccum mempty mappend
-- Like total, but with arbitrary monoids instead of number and addition

cqProd :: forall a. Num a => Cirq a a
cqProd = cqAccum 1 (*)
-- Like total, but with multiplication
-- The multiplicative dentity is 1, so it starts with 1.

cqIndex :: forall a b. Num b => b -> Cirq a b
cqIndex n = Cirq $ \_ -> (cqIndex (n + 1), n)
-- Ignores input and returns parameter, replacement Cirq returns a value one higher than the previous value.
-- Effectively indexes when applied to lists using runCq.

cqMean :: forall a. Fractional a => Cirq a a
cqMean = (cqTotal &&& cqIndex 1) >>> arr (uncurry (/))
-- Keeps two accumulators, one keeping a total and another keeping an index,
-- then divides total by the index.
-- Effectively calculates the mean of number passed so far when applied using runCq.

-- ! The concept of Cirq and many functions here are lifted from the Arrow Tutorial from Wikibooks' Hasell book.