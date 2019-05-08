{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, AllowAmbiguousTypes #-}

module Extra.Tuple (just, just', justC, map, map', mapC, mapB, sort, sort', reverse) where
-- For map and its cousins, consider importing Extra.Bifunctor instead
-- To solely import just, write import Extra.Tuple (just)

import Data.Kind (Constraint)

sort :: Ord a => (a, a) -> (a, a)
sort (x, y) = case compare x y of
    LT -> (x, y)
    EQ -> (x, y)
    GT -> (y, x)

sort' :: Ord a => (a, a) -> (a, a)
sort' (x, y) = case compare x y of
    LT -> (y, x)
    EQ -> (x, y)
    GT -> (x, y)

reverse :: (a, b) -> (b, a)
reverse (x, y) = (y, x)

just :: a -> (a, a)
just a = (a, a)
-- Creates a tuple with identical elements
-- "duplicates" the value

just' :: forall b c. (forall a. a) -> (b, c)
just' a = (a, a)
-- Creates a tuple from a universally polymorphic value.
-- Like just, but for universally polymorphic values.
-- universally polymorphic = exists for any type

justC :: forall (f :: * -> Constraint) b c. (f b, f c) => (forall a. f a => a) -> (b, c)
justC a = (a, a)
-- Like just and just', but for constrainedly polymorphic values, results in a constrainedly polymorphic tuple.
-- constrained = instance of a given class, or here, in the case of a tuple, containing them.

map :: (a -> b) -> (a, a) -> (b, b)
map f (x, y) = (f x, f y)
-- Maps a function onto a homogenous tuple
-- homogenous = same type in both slots

map' :: (forall a. a -> b) -> (x, y) -> (b, b)
map' f (x, y) = (f x, f y)
-- Maps a universally polymorphic over any (even heterogenous) tuple
-- universally polymorphic = works for any type

mapC :: forall (f :: * -> Constraint) b x y. (f x, f y) => (forall a. f a => a -> b) -> (x, y) -> (b, b)
mapC f (x, y) = (f x, f y)
-- Maps a constrainedly polymorphic over a constrained tuple
-- constrained = instance of a given class, or here, in the case of a tuple, containing them.

mapB :: forall (f :: * -> * -> Constraint) x y z w. (f x z, f y w) => (forall a b. f a b => a -> b) -> (x, y) -> (z, w)
mapB f (x, y) = (f x, f y)
-- Maps a coercion function over a tuple of coercibles
-- coercion = function from something to something else