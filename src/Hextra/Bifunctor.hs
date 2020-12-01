{-# LANGUAGE RankNTypes, ConstraintKinds, KindSignatures, AllowAmbiguousTypes, ExplicitForAll #-}

-- | More general versions of some functions from Hextra.Tuple
module Hextra.Bifunctor where

import Data.Bifunctor as Bifun
import Data.Kind as Kind

bothmap :: forall g a b. Bifunctor g => (a -> b) -> g a a -> g b b
bothmap f = bimap f f
-- ^ Maps a function onto a homogenous bifunctor (like tuples).
-- homogenous = same type in both slots

bothmap' :: forall g b x y. Bifunctor g => (forall a. a -> b) -> g x y -> g b b
bothmap' f = bimap f f
-- ^ Maps a universally polymorphic over any (even heterogenous) bifunctor.
-- universally polymorphic = works for any type

bothmapC :: forall g b (f :: Type -> Constraint) x y. (f x, f y, Bifunctor g) => (forall a. f a => a -> b) -> g x y -> g b b
bothmapC f = bimap f f
-- ^ Maps a constrainedly polymorphic over a constrained bifunctor.
-- constrained = instance of a given class, or here, in the case of a bifunctor, containing them

bothmapB :: forall g (f :: Type -> Type -> Constraint) x y z w. (f x z, f y w, Bifunctor g) => (forall a b. f a b => a -> b) -> g x y -> g z w
bothmapB f = bimap f f
-- ^ Maps a coercion function over a bifunctor of coercibles.
-- coercion = function from something to something else