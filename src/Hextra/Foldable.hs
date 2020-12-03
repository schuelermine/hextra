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

foldrUntil1 :: Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil1 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p y then (False, y) else (True, f x y)

foldrUntil2 :: Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrUntil2 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p x then (False, y) else (True, f x y)

foldrUntil3 :: Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil3 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p (f x y) then (False, y) else (True, f x y)

foldrUntil4 :: Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil4 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = (not (p y), f x y)

foldrUntil5 :: Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrUntil5 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = (not (p x), f x y)

foldrUntil6 :: Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil6 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) =  (not (p (f x y)), f x y)
