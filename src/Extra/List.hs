{-# LANGUAGE ExplicitForAll, TypeApplications #-}

module Extra.List where

import Extra
import Data.List

index :: forall n a. Integral n => [a] -> [(n, a)]
index = index' 0
  where
    index' _ [] = []
    index' n (x:xs) = (n, x) : index' (succ n) xs

replaceIndex :: forall n a. Integral n => n -> a -> [a] -> [a]
replaceIndex a = modifyIndex a . const

replaceItem :: forall a. Eq a => a -> a -> [a] -> [a]
replaceItem a b = (replace a b <$>)

modifyIndex :: forall n a. Integral n => n -> (a -> a) -> [a] -> [a]
modifyIndex _ _ [] = []
modifyIndex 0 f (x:xs) = f x : xs
modifyIndex n f l@(x:xs)
    | n <= 0 = l
    | True = x : modifyIndex (pred n) f xs

modifyItemIf :: forall a. (a -> Bool) -> (a -> a) -> [a] -> [a]
modifyItemIf p f = (applyIf p f <$>)

replaceFromIndex :: forall n a. Integral n => n -> [a] -> [a] -> [a]
replaceFromIndex _ __ [] = []
replaceFromIndex n ys (x:xs)
    | n <= 0 = ys
    | True = x : replaceFromIndex (pred n) ys xs

replaceFromWhen :: forall a. (a -> Bool) -> [a] -> [a] -> [a]
replaceFromWhen _ __ [] = []
replaceFromWhen p ys (x:xs)
    | p x = ys
    | True = x : replaceFromWhen p ys xs

replaceUpTo :: forall n a. Integral n => n -> [a] -> [a] -> [a]
replaceUpTo n ys xs = ys ++ genericDrop n xs

replaceUntil :: forall a. (a -> Bool) -> [a] -> [a] -> [a]
replaceUntil p ys xs = ys ++ dropUntil p xs

takeUntil :: forall a. (a -> Bool) -> [a] -> [a]
takeUntil = takeWhile . (not .)

dropUntil :: forall a. (a -> Bool) -> [a] -> [a]
dropUntil = dropWhile . (not .)

-- TODO replaceUntil functions