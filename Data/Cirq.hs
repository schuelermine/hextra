module Cirq (Cirq(Cirq), unCirq, cqId, cqDot, cqArr, cqFirst, cqRun, cqAccumF, cqAccum, cqWhileThen, cqWhile, cqUntilThen, cqUntil, cqTotal, cqTotalM, cqProd, cqIndex, cqMean) where
-- Reexports all unique functions defined in Cirq modules for convenience
-- * Does not define functions not defined in other Cirq modules
-- * Other Cirq modules do not define functions not exported here

import Cirq.Base
import Cirq.Tools
import Cirq.Utils