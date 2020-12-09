{-# LANGUAGE ExplicitForAll #-}

module Hextra.Foldable where

import Control.Applicative
import Hextra.Maybe

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

headF :: forall t a. Foldable t => a -> t a -> a
headF = foldr const

findP :: forall t b. (Foldable t, Functor t) => (b -> Bool) -> t b -> Maybe b
findP p ys = foldr (<|>) Nothing $ assert1 p <$> ys

--

foldrUntil1 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil1 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p y then (False, y) else (True, f x y)

foldrUntil2 :: forall t a b. Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrUntil2 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p x then (False, y) else (True, f x y)

foldrUntil3 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil3 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p (f x y) then (False, y) else (True, f x y)

foldrUntil4 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil4 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = (not (p y), f x y)

foldrUntil5 :: forall t a b. Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrUntil5 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = (not (p x), f x y)

foldrUntil6 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil6 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) =  (not (p (f x y)), f x y)

foldrUntil7 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil7 f p i = foldr g (p i, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p y then (False, y) else (True, f x y)

foldrUntil8 :: forall t a b. Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrUntil8 f p i = foldr g (True, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p x then (False, y) else (True, f x y)

foldrUntil9 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil9 f p i = foldr g (p i, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = if p (f x y) then (False, y) else (True, f x y)

foldrUntil10 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil10 f p i = foldr g (p i, i) where
    g _ (False, y) = (False, y)
    g x (True, y) = (not (p y), f x y)

foldrUntil11 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrUntil11 f p i = foldr g (p i, i) where
    g _ (False, y) = (False, y)
    g x (True, y) =  (not (p (f x y)), f x y)

foldrAny1 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny1 f p i = foldr g (True, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = if p y then (False, y) else (True, f x y)

foldrAny2 :: forall t a b. Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrAny2 f p i = foldr g (True, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = if p x then (False, y) else (True, f x y)

foldrAny3 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny3 f p i = foldr g (True, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = if p (f x y) then (False, y) else (True, f x y)

foldrAny4 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny4 f p i = foldr g (True, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = (not (p y), f x y)

foldrAny5 :: forall t a b. Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrAny5 f p i = foldr g (True, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = (not (p x), f x y)

foldrAny6 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny6 f p i = foldr g (True, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) =  (not (p (f x y)), f x y)

foldrAny7 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny7 f p i = foldr g (p i, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = if p y then (False, y) else (True, f x y)

foldrAny8 :: forall t a b. Foldable t => (a -> b -> b) -> (a -> Bool) -> b -> t a -> (Bool, b)
foldrAny8 f p i = foldr g (True, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = if p x then (False, y) else (True, f x y)

foldrAny9 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny9 f p i = foldr g (p i, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = if p (f x y) then (False, y) else (True, f x y)

foldrAny10 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny10 f p i = foldr g (p i, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) = (not (p y), f x y)

foldrAny11 :: forall t a b. Foldable t => (a -> b -> b) -> (b -> Bool) -> b -> t a -> (Bool, b)
foldrAny11 f p i = foldr g (p i, i) where
    g x (False, y) = (False, f x y)
    g x (True, y) =  (not (p (f x y)), f x y)
