{-# LANGUAGE RankNTypes, ConstraintKinds, MultiParamTypeClasses, KindSignatures, AllowAmbiguousTypes #-}

module Extra.Bifunctor where
-- More general versions of some functions from Extra.Tuple

import Data.Bifunctor as Bifun
import Data.Kind as Kind (Constraint)

mapAll :: Bifunctor g => (a -> b) -> g a a -> g b b
mapAll f x = bimap f f x
-- Maps a function onto a homogenous bifunctor (like tuples)
-- homogenous = same type in both slots

mapAll' :: Bifunctor g => (forall a. a -> b) -> g x y -> g b b
mapAll' f x = bimap f f x
-- Maps a universally polymorphic over any (even heterogenous) bifunctor
-- universally polymorphic = works for any type

mapAllC :: forall g b (f :: * -> Constraint) x y. (f x, f y, Bifunctor g) => (forall a. f a => a -> b) -> g x y -> g b b
mapAllC f x = bimap f f x
-- Maps a constrainedly polymorphic over a constrained bifunctor
-- constrained = instance of a given class, or here, in the case of a bifunctor, containing them.

mapAllB :: forall g (f :: * -> * -> Constraint) x y z w. (f x z, f y w, Bifunctor g) => (forall a b. f a b => a -> b) -> g x y -> g z w
mapAllB f x = bimap f f x
-- Maps a coercion function over a bifunctor of coercibles
-- coercion = function from something to something else