{-# LANUAGE ExplicitForAll #-}

module Extra.List where

replaceTail :: forall a. [a] -> [a] -> [a]
replaceTail [] _ = []
replaceTail (x:_) xs = x:xs
-- Fuction with as of now unknown use cases
-- Replaces the tail of a list with another list
-- The empty list has no tail, and is left untouched
-- tail = value in the top-level constructor