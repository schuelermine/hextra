{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, AllowAmbiguousTypes, ExplicitForAll, RankNTypes #-}

module Extra.Functors where

import Prelude as P (Either, const, uncurry, ($), (.), id, flip)
--import Extra.Tuple (dupe) -- (used to be for dupe f <<$>>)

class Functor f where
    map :: forall a b. (a -> b) -> f a -> f b
    (<$) :: forall a b. a -> f b -> f a
    (<$) = map . const
    {-# MINIMAL map #-}

class Bifunctor f where
    bimap :: forall a b x y. (a -> x) -> (b -> y) -> f a b -> f x y

class ContravariantBifunctor f where
    contrabiamap :: forall a b x y. (a -> x) -> (b -> y) -> f x y -> f a b

class Profunctor f where
    promap :: forall a b x y. (a -> b) -> (x -> y) -> f b x -> f a y

class ContravariantFunctor f where
    contramap :: forall a b. (a -> b) -> f b -> f a

class Invariant f where
    invmap :: forall a b. (a -> b) -> (b -> a) -> f a -> f b

class Unfunctor f where
    unmap :: forall a b. (f a -> f b) -> a -> b

class (Functor f, Unfunctor f) => SplitFullFunctor f

class (Functor f, Unfunctor f) => SplitFaithfulFunctor f

class (SplitFaithfulFunctor f, SplitFullFunctor f) => SplitFullyFaithfulFunctor f
instance (SplitFaithfulFunctor f, SplitFullFunctor f) => SplitFullyFaithfulFunctor f

class Functor f => ApplicativeFunctor f where
    pure :: forall a. a -> f a
    unit :: f ()
    (<*>) :: forall a b. f (a -> b) -> f a -> f b
    (<:>) :: forall a b. f a -> f b -> f (a, b)
    (*>) :: forall a b. f a -> f b -> f b
    (<*) :: forall a b. f a -> f b -> f a
    pure x = map (const x) unit
    unit = pure ()
    f <*> x = map (uncurry ($)) (f <:> x)
    x <:> y = (map (,) x) <*> y
    x *> y = (map (flip const) x) <*> y
    x <* y = (map (const) x) <*> y
    {-# MINIMAL (pure | unit), ((<*>) | (<:>)) #-}

class Functor f => Apply f where
    apply :: forall a b. f (a -> b) -> f a -> f b
    multiply :: forall a b. f a -> f b -> f (a, b)
    andthen :: forall a b. f a -> f b -> f b
    after :: forall a b. f a -> f b -> f a
    apply f x = map (uncurry ($)) (multiply f x)
    multiply x y = apply (map (,) x) y
    andthen x y = apply (map (flip const) x) y
    after x y = apply (map const x) y
    {-# MINIMAL apply | multiply #-}

class DecisiveFunctor f where
    decide :: f (Either a b) -> Either (f a) (f b)

class Unmultiply f where
    unmultiply :: f (a, b) -> (f a, f b)

class UnapplicativeFunctor f where
    (>:<) :: f (a, b) -> ((f a), (f b))
    unpure :: f a -> a

class Bifunctor f => BiapplicativeFunctor f where
    bipure :: a -> b -> f a b
    biunit :: f () ()
    (<<*>>) :: f (a -> x) (b -> y) -> f a b -> f x y
    (<<:>>) :: f a b -> f x y -> f (a, x) (b, y)
    (*>>) :: f a b -> f x y -> f x y
    (<<*) :: f a b -> f x y -> f a b
    bipure x y = bimap (const x) (const y) biunit
    biunit = bipure () ()
    f <<*>> x = bimap (uncurry ($)) (uncurry ($)) (f <<:>> x)
    x <<:>> y = (bimap (,) (,) x) <<*>> y
    x *>> y = bimap (flip const) (flip const) x <<*>> y
    x <<* y = bimap (const) (const) x <<*>> y
    {-# MINIMAL (bipure | biunit), ((<<*>>), (<<:>>)) #-}

class Bifunctor f => Biapply f where
    biapply :: f (a -> x) (b -> y) -> f a b -> f x y
    bimultiply :: f a b -> f x y -> f (a, x) (b, y)
    biapply f x = bimap (uncurry ($)) (uncurry ($)) (bimultiply f x)
    bimultiply x y = biapply (bimap (,) (,) x) y 

class ApplicativeFunctor m => Monad m where
    return :: a -> m a
    join :: m (m a) -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
    return = pure
    join = (>>= id)
    m >>= f = join $ map f m
    f >=> g = \x -> f x >>= g
    {-# MINIMAL (>>=) | join#-}

class Functor w => Comonad w where
    extract :: w a -> a
    expand :: w a -> w (w a)
    (=>>) :: w a -> (w a -> b) -> w b
    (=>=) :: (w a -> b) -> (w b -> c) -> w a -> c
    expand = (=>> id)
    x =>> f = map f (expand x)
    f =>= g = g . (=>> f)
    {-# MINIMAL extract , (expand | (=>>)) #-}

class (Bifunctor l, Bifunctor r) => Bimonad l r where
    bireturnl :: a -> l a b
    bireturnr :: b -> r a b
    bijoinl :: l (l a b) (r a b) -> l a b
    bijoinr :: r (l a b) (r a b) -> r a b
    bibindl :: l a b -> (a -> l x y) -> (b -> r x y) -> l x y
    bibindr :: r a b -> (a -> l x y) -> (b -> r x y) -> r x y

(<#>) = contramap
(<$>) = map
(<<$>>) = uncurry bimap
(>$<) = unmap