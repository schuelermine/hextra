module Data.Cirq.Deprecated () where
-- Archive for old functions, prototypes and other stuff
-- Functionality of these functions is usually still available

import Data.Cirq.Base as Cq

cquntilIncB :: (b -> Bool) -> Cirq a b -> Cirq a b -> Cirq a b
cquntilIncB c cq (Cirq q) = Cirq $ \a ->
    let (q', b) = q a
        cq' = if c b then cq else cquntilIncB c cq q'
    in  (cq', b)

cquntilExA :: (a -> Bool) -> Cirq a b -> Cirq a b -> Cirq a b
cquntilExA c cq (Cirq q) = Cirq $ \a ->
    let (q', b) = q a
        cq' = if c a then cq else cquntilExA c cq q'
    in  (cq', b)

-- cquntilExB :: (b -> Bool) -> Cirq a b -> Cirq a b -> Cirq a b)