{-# LANGUAGE ExplicitForAll #-}

module Hextra.Foldable where

safeFoldr1 :: forall t a. Foldable t => (a -> a -> a) -> t a -> Maybe a
safeFoldr1 f = foldr q Nothing where
    q x Nothing = Just x
    q x (Just y) = Just $ f x y

safeMaximum :: forall t a. (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = safeFoldr1 max
safeMinimum :: forall t a. (Foldable t, Ord a) => t a -> Maybe a
safeMinimum = safeFoldr1 min

weigh :: forall t n a. (Foldable t, Num n) => t (n, a -> Bool) -> a -> n
weigh ws x = foldr ((+) . f) 0 ws where
    f (n, p) = if p x then n else 0
