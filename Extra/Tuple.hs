{-# LANGUAGE RankNTypes, ConstraintKinds, MultiParamTypeClasses, KindSignatures, AllowAmbiguousTypes #-}

module Extra.Tuple (dupe, mapAll, mapAll', mapAllC, mapAllB, tSort, tSort', tReverse) where
-- For mapAll and its cousins, consider importing Extra.Bifunctor instead
-- To solely import dupe, write import Extra.Tuple (dupe)

import Data.Kind (Constraint)

tSort :: Ord a => (a, a) -> (a, a)
tSort (x, y) = case compare x y of
    LT -> (x, y)
    EQ -> (x, y)
    GT -> (y, x)

tSort' :: Ord a => (a, a) -> (a, a)
tSort' (x, y) = case compare x y of
    LT -> (y, x)
    EQ -> (x, y)
    GT -> (x, y)

tReverse :: (a, b) -> (b, a)
tReverse (x, y) = (y, x)

dupe :: a -> (a, a)
dupe a = (a, a)
-- Creates a tuple with identical elements
-- "duplicates" the value

mapAll :: (a -> b) -> (a, a) -> (b, b)
mapAll f (x, y) = (f x, f y)
-- Maps a function onto a homogenous tuple
-- homogenous = same type in both slots

mapAll' :: (forall a. a -> b) -> (x, y) -> (b, b)
mapAll' f (x, y) = (f x, f y)
-- Maps a universally polymorphic over any (even heterogenous) tuple
-- universally polymorphic = works for any type

mapAllC :: forall b (f :: * -> Constraint) x y. (f x, f y) => (forall a. f a => a -> b) -> (x, y) -> (b, b)
mapAllC f (x, y) = (f x, f y)
-- Maps a constrainedly polymorphic over a constrained tuple
-- constrained = instance of a given class, or here, in the case of a tuple, containing them.

mapAllB :: forall (f :: * -> * -> Constraint) x y z w. (f x z, f y w) => (forall a b. f a b => a -> b) -> (x, y) -> (z, w)
mapAllB f (x, y) = (f x, f y)
-- Maps a coercion function over a tuple of coercibles
-- coercion = function from something to something else