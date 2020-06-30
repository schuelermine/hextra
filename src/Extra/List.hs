{-# LANGUAGE ExplicitForAll, TypeApplications #-}

module Extra.List where

import Extra

index :: forall n a. Integral n => [a] -> [(n, a)]
index = index' 0
  where
    index' _ [] = []
    index' n (x:xs) = (n, x) : index' (n + 1) xs

replaceIndex :: forall n a. Integral n => n -> a -> [a] -> [a]
replaceIndex a = modifyIndex a . const

replaceItem :: forall a. Eq a => a -> a -> [a] -> [a]
replaceItem a b = (replace a b <$>)

modifyIndex :: forall n a. Integral n => n -> (a -> a) -> [a] -> [a]
modifyIndex _ _ [] = []
modifyIndex 0 f (x:xs) = f x : xs
modifyIndex n f l@(x:xs)
    | n <= 0 = l
    | True = x : modifyIndex (n - 1) f xs

replaceTail :: forall a. [a] -> [a] -> [a]
replaceTail [] _ = []
replaceTail (x:_) xs = x:xs
-- Fuction with as of now unknown use cases
-- Replaces the tail of a list with another list.
-- The empty list has no tail, and is left untouched.
-- tail = value in the top-level constructor