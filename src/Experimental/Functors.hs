{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, AllowAmbiguousTypes, ExplicitForAll, RankNTypes #-}

module Experimental.Functors where

import Prelude as P hiding (Functor, Monad, map, (<$), pure, (<*>), return, (>>=), (<$>), (*>), (<*))

class Functor f where
    map :: forall a b. (a -> b) -> f a -> f b
    (<$) :: forall a b. a -> f b -> f a
    (<$) = map . const
    {-# MINIMAL map #-}

class Bifunctor f where
    bimap :: forall a b x y. (a -> x) -> (b -> y) -> f a b -> f x y

class ContravariantBifunctor f where
    contrabimap :: forall a b x y. (a -> x) -> (b -> y) -> f x y -> f a b

class Profunctor f where
    promap :: forall a b x y. (a -> b) -> (x -> y) -> f b x -> f a y

class ContravariantFunctor f where
    contramap :: forall a b. (a -> b) -> f b -> f a

class InvariantFunctor f where
    invmap :: forall a b. (a -> b) -> (b -> a) -> f a -> f b

class Unfunctor f where
    unmap :: forall a b. (f a -> f b) -> a -> b

class Unbifunctor f where
    unbimap :: forall a b x y. (f a b -> f x y) -> ((a -> x), (b -> y))

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
    decide :: forall a b. f (Either a b) -> Either (f a) (f b)

class DecisiveUnfunctor f where
    undecide :: forall a b. Either (f a) (f b) -> f (Either a b)

class Unmultiply f where
    unmultiply :: forall a b. f (a, b) -> (f a, f b)

class UnapplicativeFunctor f where
    (>:<) :: forall a b. f (a, b) -> ((f a), (f b))
    unpure :: forall a. f a -> a

class Bifunctor f => BiapplicativeFunctor f where
    bipure :: forall a b. a -> b -> f a b
    biunit :: f () ()
    (<<*>>) :: forall a b x y. f (a -> x) (b -> y) -> f a b -> f x y
    (<<:>>) :: forall a b x y. f a b -> f x y -> f (a, x) (b, y)
    (*>>) :: forall a b x y. f a b -> f x y -> f x y
    (<<*) :: forall a b x y. f a b -> f x y -> f a b
    bipure x y = bimap (const x) (const y) biunit
    biunit = bipure () ()
    f <<*>> x = bimap (uncurry ($)) (uncurry ($)) (f <<:>> x)
    x <<:>> y = (bimap (,) (,) x) <<*>> y
    x *>> y = bimap (flip const) (flip const) x <<*>> y
    x <<* y = bimap (const) (const) x <<*>> y
    {-# MINIMAL (bipure | biunit), ((<<*>>) | (<<:>>)) #-}

class Bifunctor f => Biapply f where
    biapply :: forall a b x y. f (a -> x) (b -> y) -> f a b -> f x y
    bimultiply :: forall a b x y. f a b -> f x y -> f (a, x) (b, y)
    biapply f x = bimap (uncurry ($)) (uncurry ($)) (bimultiply f x)
    bimultiply x y = biapply (bimap (,) (,) x) y 

class ApplicativeFunctor m => Monad m where
    return :: forall a. a -> m a
    join :: forall a. m (m a) -> m a
    (>>=) :: forall a b. m a -> (a -> m b) -> m b
    (>=>) :: forall a b c. (a -> m b) -> (b -> m c) -> a -> m c
    return = pure
    join = (>>= id)
    m >>= f = join $ map f m
    f >=> g = \x -> f x >>= g
    {-# MINIMAL (>>=) | join #-}

class Functor w => Comonad w where
    extract :: forall a. w a -> a
    expand :: forall a. w a -> w (w a)
    (=>>) :: forall a b. w a -> (w a -> b) -> w b
    (=>=) :: forall a b c. (w a -> b) -> (w b -> c) -> w a -> c
    expand = (=>> id)
    x =>> f = map f (expand x)
    f =>= g = g . (=>> f)
    {-# MINIMAL extract, (expand | (=>>)) #-}

class (Bifunctor l, Bifunctor r) => Bimonad l r where
    bireturnl :: forall a b. a -> l a b
    bireturnr :: forall a b. b -> r a b
    bijoinl :: forall a b. l (l a b) (r a b) -> l a b
    bijoinr :: forall a b. r (l a b) (r a b) -> r a b
    bibindl :: forall a b x y. l a b -> (a -> l x y) -> (b -> r x y) -> l x y
    bibindr :: forall a b x y. r a b -> (a -> l x y) -> (b -> r x y) -> r x y
    bibindl x f g = bijoinl (bimap f g x)
    bibindr x f g = bijoinr (bimap f g x)
    bijoinl x = bibindl x id id
    bijoinr x = bibindr x id id
    {-# MINIMAL bireturnl, bireturnr, ((bijoinl, bijoinr) | (bibindl, bibindr)) #-}

class Bimonad m m => JointBimonad m
instance Bimonad m m => JointBimonad m

--class Cobimonad l r where
--    biextractl :: forall a b. l a b -> a
--    biextractr :: forall a b. r a b -> b

(<#>) = contramap
(<$>) = map
(<<$>>) = uncurry bimap
(>$<) = unmap