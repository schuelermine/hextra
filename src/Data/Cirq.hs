module Data.Cirq (Cirq(Cirq), unCirq, cqId, cqDot, cqArr, cqFirst, cqRun, cqAccumF, cqAccum, cqWhileThen, cqWhile, cqUntilThen, cqUntil, cqTotal, cqTotalM, cqProd, cqIndex, cqMean) where
-- ^ Reexports all unique functions defined in Cirq modules for convenience.
-- Does not define functions not defined in other Cirq modules

-- Other Cirq modules do not define functions not exported here
-- TODO ^ Clarify

import Data.Cirq.Base
import Data.Cirq.Tools
import Data.Cirq.Utils

-- ! The concept of this is lifted from the Arrow Tutorial from Wikibooks' Hasell book.