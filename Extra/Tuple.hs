{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, AllowAmbiguousTypes #-}

module Extra.Tuple where
-- For mapAll and its cousins, consider importing Extra.Bifunctor instead
-- To solely import dupe, write import Extra.Tuple (dupe)

import Data.Kind as Kind

tSort :: Ord a => (a, a) -> (a, a)
tSort (x, y) = case compare x y of
    LT -> (x, y)
    EQ -> (x, y)
    GT -> (y, x)
-- Sorts a tuple

tSort' :: Ord a => (a, a) -> (a, a)
tSort' (x, y) = case compare x y of
    LT -> (y, x)
    EQ -> (x, y)
    GT -> (x, y)
-- Sorts a tuple, reversed

tReverse :: (a, b) -> (b, a)
tReverse (x, y) = (y, x)

dupe :: a -> (a, a)
dupe a = (a, a)
-- Creates a tuple with identical elements
-- "duplicates" the value

dupe' :: forall b c. (forall a. a) -> (b, c)
dupe' a = (a, a)
-- Creates a tuple from a universally polymorphic value.
-- Like dupe, but for universally polymorphic values.
-- universally polymorphic = exists for any type

dupeC :: forall (f :: * -> Constraint) b c. (f b, f c) => (forall a. f a => a) -> (b, c)
dupeC a = (a, a)
-- Like dupe and dupe', but for constrainedly polymorphic values, results in a constrainedly polymorphic tuple.
-- constrained = instance of a given class, or here, in the case of a tuple, containing them.

mapAll :: (a -> b) -> (a, a) -> (b, b)
mapAll f (x, y) = (f x, f y)
-- Maps a function onto a homogenous tuple
-- homogenous = same type in both slots

mapAll' :: (forall a. a -> b) -> (x, y) -> (b, b)
mapAll' f (x, y) = (f x, f y)
-- Maps a universally polymorphic over any (even heterogenous) tuple
-- universally polymorphic = works for any type

mapAllC :: forall (f :: * -> Constraint) b x y. (f x, f y) => (forall a. f a => a -> b) -> (x, y) -> (b, b)
mapAllC f (x, y) = (f x, f y)
-- Maps a constrainedly polymorphic over a constrained tuple
-- constrained = instance of a given class, or here, in the case of a tuple, containing them.

mapAllB :: forall (f :: * -> * -> Constraint) x y z w. (f x z, f y w) => (forall a b. f a b => a -> b) -> (x, y) -> (z, w)
mapAllB f (x, y) = (f x, f y)
-- Maps a coercion function over a tuple of coercibles
-- coercion = function from something to something else