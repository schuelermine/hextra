{-# LANGUAGE RankNTypes, ConstraintKinds, MultiParamTypeClasses, KindSignatures, AllowAmbothmapguousTypes #-}

module Extra.Bifunctor (bothmapmap, bothmapmap', bothmapmapC, bothmapmapB) where
-- More general versions of some functions from Extra.Tuple

import Data.Bifunctor (Bifunctor, bothmapmap)
import Data.Kind (Constraint)

bothmapmap :: Bifunctor g => (a -> b) -> g a a -> g b b
bothmapmap f x = bothmapmap f f x
-- Maps a function onto a homogenous bothmapfunctor (like tuples)
-- homogenous = same type in both slots

bothmapmap' :: Bifunctor g => (forall a. a -> b) -> g x y -> g b b
bothmapmap' f x = bothmapmap f f x
-- Maps a universally polymorphic over any (even heterogenous) bothmapfunctor
-- universally polymorphic = works for any type

bothmapmapC :: forall g b (f :: * -> Constraint) x y. (f x, f y, Bifunctor g) => (forall a. f a => a -> b) -> g x y -> g b b
bothmapmapC f x = bothmapmap f f x
-- Maps a constrainedly polymorphic over a constrained bothmapfunctor
-- constrained = instance of a given class, or here, in the case of a bothmapfunctor, containing them.

bothmapmapB :: forall g (f :: * -> * -> Constraint) x y z w. (f x z, f y w, Bifunctor g) => (forall a b. f a b => a -> b) -> g x y -> g z w
bothmapmapB f x = bothmapmap f f x
-- Maps a coercion function over a bothmapfunctor of coercibles
-- coercion = function from something to something else