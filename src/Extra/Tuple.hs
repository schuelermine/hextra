{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, AllowAmbiguousTypes, ExplicitForAll #-}

module Extra.Tuple where
-- ^ For something like tBothmap and its cousins, consider importing Extra.Bifunctor instead
-- To solely import dupe, write import Extra.Tuple (dupe)

import Data.Kind as Kind

tSort :: forall a. Ord a => (a, a) -> (a, a)
tSort (x, y) = case compare x y of
    LT -> (x, y)
    EQ -> (x, y)
    GT -> (y, x)
-- ^ Sorts a tuple

tSort' :: forall a. Ord a => (a, a) -> (a, a)
tSort' (x, y) = case compare x y of
    LT -> (y, x)
    EQ -> (x, y)
    GT -> (x, y)
-- ^ Sorts a tuple, reversed

tReverse :: forall a b. (a, b) -> (b, a)
tReverse (x, y) = (y, x)

dupe :: forall a. a -> (a, a)
dupe a = (a, a)
-- ^ Creates a tuple with identical elements.
-- "Duplicates" the value.

dupe' :: forall b c. (forall a. a) -> (b, c)
dupe' a = (a, a)
-- ^ Creates a tuple from a universally polymorphic value.
-- Like dupe, but for universally polymorphic values
-- universally polymorphic = exists for any type

dupeC :: forall (f :: Type -> Constraint) b c. (f b, f c) => (forall a. f a => a) -> (b, c)
dupeC a = (a, a)
-- ^ Like dupe and dupe', but for constrainedly polymorphic values, results in a constrainedly polymorphic tuple
-- constrained = instance of a given class, or here, in the case of a tuple, containing them

tBothmap :: forall a b. (a -> b) -> (a, a) -> (b, b)
tBothmap f (x, y) = (f x, f y)
-- ^ Maps a function onto a homogenous tuple.
-- homogenous = same type in both slots

tBothmap' :: forall b x y. (forall a. a -> b) -> (x, y) -> (b, b)
tBothmap' f (x, y) = (f x, f y)
-- ^ Maps a universally polymorphic over any (even heterogenous) tuple.
-- universally polymorphic = works for any type

tBothmapC :: forall b (f :: Type -> Constraint) x y. (f x, f y) => (forall a. f a => a -> b) -> (x, y) -> (b, b)
tBothmapC f (x, y) = (f x, f y)
-- ^ Maps a constrainedly polymorphic over a constrained tuple.
-- constrained = instance of a given class, or here, in the case of a tuple, containing them

tBothmapB :: forall (f :: Type -> Type -> Constraint) x y z w. (f x z, f y w) => (forall a b. f a b => a -> b) -> (x, y) -> (z, w)
tBothmapB f (x, y) = (f x, f y)
-- ^ Maps a coercion function over a tuple of coercibles.
-- coercion = function from something to something else