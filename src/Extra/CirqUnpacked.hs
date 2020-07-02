{-# LANGUAGE ExplicitForAll #-}

-- | Exports Cirqs run through cqRun, runnable on lists (type like: [a] -> [b]).
-- See Data.Cirq.Utils for implementation.
module Extra.CirqUnpacked where

import Data.Cirq as Cq

total :: forall a. Num a => [a] -> [a]
total = cqRun cqTotal

totalM :: forall a. Monoid a => [a] -> [a]
totalM = cqRun cqTotalM

prod :: forall a. Num a => [a] -> [a]
prod = cqRun cqProd

mean :: forall a. Fractional a => [a] -> [a]
mean = cqRun cqMean

-- See Data.Cirq.Utils for implementation.