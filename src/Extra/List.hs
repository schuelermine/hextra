{-# LANGUAGE ExplicitForAll, TypeApplications #-}

module Extra.List where

replaceIndex :: forall n a. Integral n => n -> a -> [a] -> [a]
replaceIndex _ _ [] = []
replaceIndex 0 a (_:xs) = a : xs
replaceIndex n a l@(x:xs)
    | n <= 0 = l
    | True = x : replaceIndex (n - 1) a xs

replaceTail :: forall a. [a] -> [a] -> [a]
replaceTail [] _ = []
replaceTail (x:_) xs = x:xs
-- Fuction with as of now unknown use cases
-- Replaces the tail of a list with another list.
-- The empty list has no tail, and is left untouched.
-- tail = value in the top-level constructor