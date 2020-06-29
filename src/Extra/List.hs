{-# LANGUAGE ExplicitForAll, TypeApplications #-}

module Extra.List where

genericReplace :: forall n a. Integral n => n -> a -> [a] -> [a]
genericReplace _ _ [] = []
genericReplace 0 a (_:xs) = a : xs
genericReplace n a l@(x:xs)
    | n <= 0 = l
    | True = x : genericReplace (n - 1) a xs

replace = genericReplace @Int
-- TODO Debate if this should be Integer instead.



replaceTail :: forall a. [a] -> [a] -> [a]
replaceTail [] _ = []
replaceTail (x:_) xs = x:xs
-- Fuction with as of now unknown use cases
-- Replaces the tail of a list with another list.
-- The empty list has no tail, and is left untouched.
-- tail = value in the top-level constructor