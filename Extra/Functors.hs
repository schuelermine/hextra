{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}

module Extra.Functors where

import Prelude as P (Either, const, uncurry, ($), (.), id)

class Functor f where
    map :: (a -> b) -> f a -> f b

class Bifunctor f where
    bimap :: (a -> x) -> (b -> y) -> f a b -> f x y

class Bicofunctor f where
    bicomap :: (f a b -> f x y) -> Either (a -> x) (b -> y)

class Bicontravariant f where
    bicontramap :: (a -> x) -> (b -> y) -> f x y -> f a b

class Profunctor f where
    promap :: (a -> b) -> (x -> y) -> f b x -> f a y

class Contrafunctor f where
    contramap :: (a -> b) -> f b -> f a

class Invariant f where
    invmap :: (a -> b) -> (b -> a) -> f a -> f b

class Cofunctor f where
    comap :: (f a -> f b) -> a -> b

class Contracofunctor f where
    contracomap :: (f a -> f b) -> b -> a

class Bicontracofunctor f where
    bicontracomap :: (f a b -> f x y) -> Either (x -> a) (y -> b)

class Procofunctor f where
    procomap :: (f a b -> f x y) -> Either (x -> a) (b -> y)

class Functor f => Applicative f where
    pure :: a -> f a
    unit :: f ()
    (<*>) :: f (a -> b) -> f a -> f b
    (<:>) :: f a -> f b -> f (a, b)
    pure x = map (const x) unit
    unit = pure ()
    f <*> x = map (uncurry ($)) (f <:> x)
    x <:> y = ((,) <$> x) <*> y
    {-# MINIMAL (pure, (<*>)) | (unit, (<:>)) #-}

class Functor f => Apply f where
    apply :: f (a -> b) -> f a -> f b

class Functor f => Coapplicative f where
    cozip :: f (Either a b) -> Either (f a) (f b)

class Bifunctor f => Biapplicative f where
    bipure :: a -> b -> f a b
    biunit :: f () ()
    biapply :: f (a -> x) (b -> y) -> f a b -> f x y
    bizip :: f a b -> f x y -> f (a, x) (b, y)
    bipure x y = bimap (const x) (const y) biunit
    biunit = bipure () ()
    bizip x y = biapply (bimap (,) (,) x) y
    biapply f x = bimap (uncurry ($)) (uncurry ($)) (bizip f x)
    {-# MINIMAL (bipure, biapply) | (biunit, bizip) #-}

class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    join :: m (m a) -> m a
    (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
    m >>= f = join $ map f m
    join = (>>= id)
    f >=> g = \x -> f x >>= g
    {-# MINIMAL return , ((>>=), join)#-}

class Functor w => Comonad w where
    extract :: w a -> a
    expand :: w a -> w (w a)
    extend :: (w a -> b) -> w a -> w b
    (=>=) :: (w a -> b) -> (w b -> c) -> w a -> c
    expand = extend id
    extend f = map f . expand
    f =>= g = g . extend f
    {-# MINIMAL extract , (expand | extend) #-}

-- class 

(<#>) = contramap
(<$>) = map