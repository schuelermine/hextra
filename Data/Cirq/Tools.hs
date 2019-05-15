{-# LANUAGE ExplicitForall #-}

module Data.Cirq.Tools where
-- Exports tools for modifying Cirqs after the fact

import Data.Cirq.Base as Cq

cqWhileThen :: forall a b. (a -> Bool) -> Cirq a b -> Cirq a b -> Cirq a b
cqWhileThen f (Cirq q1) cq2@(Cirq q2) = Cirq $ \a ->
    let (cq1', b1) = q1 a
        (cq2', b2) = q2 a
    in  case f a of
            True  -> (cqWhileThen f cq1' cq2, b1)
            False -> (cq2', b2)
-- Performs Cirq 1 if condition is true, Cirq 2 if false,
-- returns itself with the new Cirq 1 or just Cirq 2
-- Effectively performs Cirq 1 while f is true, then performs Cirq 2
-- Discards any reference to itself when Cirq 2 starts

cqWhile :: forall a b. (a -> Bool) -> Cirq a b -> Cirq a b -> Cirq a b
cqWhile f (Cirq q1) (Cirq q2) = Cirq $ \a ->
    let (cq1', b1) = q1 a
        (cq2', b2) = q2 a
    in  case f a of
            True  -> (cqWhile f cq1' cq2', b1)
            False -> (cq2', b2)
-- Like cqWhileThen, but also performs Cirq 2 even when it's output isn't being returned

cqUntilThen :: forall a b. (a -> Bool) -> Cirq a b -> Cirq a b -> Cirq a b
cqUntilThen f = cqWhileThen (not . f)
-- Like cqWhileThen, but negates the condition

cqUntil :: forall a b. (a -> Bool) -> Cirq a b -> Cirq a b -> Cirq a b
cqUntil f = cqWhile (not . f)
-- Like cqWhile, but negates the condition