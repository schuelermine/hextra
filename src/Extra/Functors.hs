{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, AllowAmbiguousTypes, ExplicitForAll, RankNTypes #-}

module Extra.Functors where

class ContravariantBifunctor f where
    contrabimap :: forall a b x y. (a -> x) -> (b -> y) -> f x y -> f a b

class Unfunctor f where
    unmap :: forall a b. (f a -> f b) -> a -> b

class Unbifunctor f where
    unbimap :: forall a b x y. (f a b -> f x y) -> ((a -> x), (b -> y))

class (Functor f, Unfunctor f) => SplitFullFunctor f

class (Functor f, Unfunctor f) => SplitFaithfulFunctor f

class (SplitFaithfulFunctor f, SplitFullFunctor f) => SplitFullyFaithfulFunctor f
instance (SplitFaithfulFunctor f, SplitFullFunctor f) => SplitFullyFaithfulFunctor f

class DecisiveFunctor f where
    decide :: forall a b. f (Either a b) -> Either (f a) (f b)

class DecisiveUnfunctor f where
    undecide :: forall a b. Either (f a) (f b) -> f (Either a b)

class Unmultiply f where
    unmultiply :: forall a b. f (a, b) -> (f a, f b)

class UnapplicativeFunctor f where
    (>:<) :: forall a b. f (a, b) -> ((f a), (f b))
    unpure :: forall a. f a -> a