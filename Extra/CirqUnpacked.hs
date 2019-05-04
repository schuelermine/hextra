module Extra.CirqUnpacked (total, totalM, prod, mean) where
-- Exports Cirqs run through cqrun, runnable on lists (type approx: [a] -> [b])

import Cirq.Basic (cqRun)
import Cirq.Utils

total :: Num a => [a] -> [a]
total = cqRun cqTotal

totalM :: Monoid a => [a] -> [a]
totalM = cqRun cqTotalM

prod :: Num a => [a] -> [a]
prod = cqRun cqProd

mean :: Fractional a => [a] -> [a]
mean = cqRun cqMean