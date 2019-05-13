{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Extra.Applicative where
-- Defines usefule and alternative applicative functions and constructs

(<:>) :: Applicative f => f a -> f b -> f (a, b)
(<:>) a b = (,) <$> a <*> b
-- Pairs up all elements in two applicative functors
-- One of the operations/values of the monoidal presentation of functors

unit :: Applicative f => f ()
unit = pure ()
-- Applicative functor with () in it
-- One of the operations/values of the monoidal presentation of functors

(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<.>) f g = (.) <$> f <*> g
-- Composes two applicative functions

mkApp :: Functor f => (forall x y. f x -> f y -> f (x, y)) -> f (a -> b) -> f a -> f b
mkApp (?) f x = fmap (uncurry ($)) $ f ? x
-- Creates a (<*>) definition from a definition of (<:>)
-- mkApp (<:>) = (<*>)

mkPure :: Functor f => (f ()) -> a -> f a
mkPure u a = fmap (const a) u
-- Creates a pure definition from a definition of unit
-- mkPure unit = pure