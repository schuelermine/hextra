module Cirq.Utils (cqTotal, cqTotalM, cqProd, cqIndex, cqMean) where
-- Exports predefined Cirqs

import Cirq.Full
import Control.Arrow

cqTotal :: Num a => Cirq a a
cqTotal = cqAccum 0 (+)
-- Keeps track of the total of numbers passed to it.
-- When a number is passed to it, the replacement function will add that number to its number,
-- and so on

cqTotalM :: Monoid a => Cirq a a
cqTotalM = cqAccum mempty mappend
-- Like total, but with arbitrary monoids instead of number and addition

cqProd :: Num a => Cirq a a
cqProd = cqAccum 1 (*)
-- Like total, but with multiplication.
-- Identity is 1, so it starts with 1

cqIndex :: Num b => b -> Cirq a b
cqIndex n = Cirq $ \_ -> (cqIndex (n + 1), n)
-- Ignores input and returns input value, replacement Cirq returns a value 1 higher than the previous value
-- Effectively indexes when applied to lists using runCq

cqMean :: Fractional a => Cirq a a
cqMean = (cqTotal &&& cqIndex 1) >>> arr (uncurry (/))
-- Keeps two accumulators, one keeping a total and another keeping an index,
-- then divides total by the index, effectively calculating mean of number passed so far