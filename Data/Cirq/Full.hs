{-# LANGUAGE Arrows #-}

module Cirq.Full (Cirq(Cirq), unCirq, cqId, cqDot, cqArr, cqFirst, cqRun, cqAccumF, cqAccum) where
-- Defines Cirq, an Arrow that always produces a replacement for itself.
-- Useful for iterating through lists, easily combinable thanks to Arrow

import Control.Arrow
import qualified Control.Category as Cat

import Extra.Tuple (dupe)
-- Helper function. dupe :: a -> (a, a)

newtype Cirq a b = Cirq { unCirq :: a -> (Cirq a b, b) }
-- A function that returns a replacement for itself alongside the result

instance Cat.Category Cirq where
    id = Cirq $ \a -> (Cat.id, a)
    -- Does nothing to the input, returns itself as replacement
    (Cirq q2) . (Cirq q1) = Cirq $ \a ->
        let (q1', b) = q1 a
            (q2', c) = q2 b
        in  (q2' Cat.. q1', c)
    -- Runs Cirq 1 on a value, gives back new Cirq 1 and another value,
    -- run Cirq 2 on that, gives back new Cirq 2 and value,
    -- return value and, as replacement, return composition of new Cirq 1 and new Cirq 2

cqId :: Cirq a a
cqId = Cat.id
cqDot :: Cirq b c -> Cirq a b -> Cirq a c
cqDot = (Cat..)
-- Specialized versions of Cat.Category functions for exporting,
-- if the main program doesn't want to or can't import Control.Category

instance Arrow Cirq where
    arr f = Cirq $ \a -> (arr f, f a)
    -- Applies function, returns itself (see Cat.id above)
    first (Cirq q) = Cirq $ \(a, b) ->
        let (q', c) = q a
        in  (first q', (c, b))
    -- Makes a new Cirq that applies the original cirq only to the first value of a pair

cqArr :: (a -> b) -> Cirq a b
cqArr = arr
cqFirst :: Cirq a b -> Cirq (a, c) (b, c)
cqFirst = first
-- Specialized versions of Arrow functions for exporting,
-- if the main program doesn't want to or can't import Control.Arrow

cqRun :: Cirq a b -> [a] -> [b]
cqRun _ [] = []
cqRun cq (x:xs) =
    let (cq', y) = unCirq cq x
    in  y : cqRun cq' xs
-- Iterates throug a list using a Cirq
-- Every step, the item is replaced by the result of the current Cirq,
-- then, the next item is processed using the new Cirq

cqAccumF :: k -> (a -> k -> (b, k)) -> Cirq a b
cqAccumF k f = Cirq $ \a ->
    let (b, k') = f a k
    in  (cqAccumF k' f, b)
-- Turns a function that can keep an accumulator value alongside the result into a Cirq
-- Doesn't ouput the accumulator, it is only used as info for the next function application

cqAccum :: k -> (a -> k -> k) -> Cirq a k
cqAccum k f = cqAccumF k (\a b -> dupe (f a b))
-- Like cqAccumF, but the output value is the accumulator