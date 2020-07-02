{-# LANGUAGE ExistentialQuantification, RankNTypes, ExplicitForAll #-}

-- | Defines useful and alternative applicative functions and constructs.
module Extra.Applicative where

infixl 5 <:>
(<:>) :: forall f a b. Applicative f => f a -> f b -> f (a, b)
(<:>) a b = (,) <$> a <*> b
-- ^ Pairs up all elements in two applicative functors.
-- One of the operations/values of the monoidal presentation of functors

infixl 4 <::>
(<::>) :: forall f a b. Applicative f => f a -> f b -> f (a, b)
(<::>) = (<:>)
-- ^ Just (<:>), but with lower precedence

infixl 6 <<>>
(<<>>) :: forall f a. (Applicative f, Monoid a) => f a -> f a -> f a
a <<>> b = mappend <$> a <*> b
-- ^ Adds up values in two applicative functors.

unit :: forall f. Applicative f => f ()
unit = pure ()
-- ^ Applicative functor with () in it
-- One of the operations/values of the monoidal presentation of functors

(<.>) :: forall f b c a. Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<.>) f g = (.) <$> f <*> g
-- ^ Composes two applicative functions.

mkApp :: forall f a b. Functor f => (forall x y. f x -> f y -> f (x, y)) -> f (a -> b) -> f a -> f b
mkApp (?) f x = fmap (uncurry ($)) $ f ? x
-- ^ Creates a (<*>) definition from a definition of (<:>).
-- mkApp (<:>) = (<*>)

mkPure :: forall f a. Functor f => (f ()) -> a -> f a
mkPure u a = fmap (const a) u
-- ^ Creates a pure definition from a definition of unit.
-- mkPure unit = pure

class Monoidal f where
    nilA :: f ()
    zipA :: f a -> f b -> f (a, b)

mkNilA :: forall f. Functor f => (forall x. x -> f x) -> f ()
mkNilA p = p ()

mkZipA :: forall f a b. Functor f => (forall x y. f (x -> y) -> f x -> f y) -> f a -> f b -> f (a, b)
mkZipA (?) x y = ((,) <$> x) ? y