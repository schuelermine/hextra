module Data.Cirq.Basic (Cirq, cqId, cqDot, cqArr, cqFirst, cqRun, cqAccumF, cqAccum) where
-- ^ Only reexports high-level functions.
-- Useful when you want to make sure Cirqs behave

import Data.Cirq.Base as Cq

-- ! The concept of this is lifted from the Arrow Tutorial from Wikibooks' Hasell book.