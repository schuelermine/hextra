{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Extra.Applicative where

(<:>) :: Applicative f => f a -> f b -> f (a, b)
(<:>) a b = (,) <$> a <*> b

unit :: Applicative f => f ()
unit = pure ()

(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<.>) f g = (.) <$> f <*> g

mkApp :: Functor f => (forall x y. f x -> f y -> f (x, y)) -> f (a -> b) -> f a -> f b
mkApp (?) f x = fmap (uncurry ($)) $ f ? x

mkPure :: Functor f => (f ()) -> a -> f a
mkPure u a = fmap (const a) u