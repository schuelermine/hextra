{-# LANGUAGE FlexibleInstances, UndecidableInstances, StandaloneDeriving, GeneralizedNewtypeDeriving #-}

module Extra.Group where

import Control.Applicative

class Magma m where
    (<+>) :: m -> m -> m

instance Semigroup m => Magma m where
    (<+>) = (<>)

class Monoid g => Group g where
    mnegate :: g -> g
    mminus :: g -> g -> g
    mnegate = mminus mempty
    mminus x y = mappend x (mnegate y)
    {-# MINIMAL (mnegate | mminus) #-}

class Group r => Rung r where
    mmult :: r -> r -> r

class Rung r => Ring r where
    mone :: r

class Ring f => Field f where
    mrecip :: f -> f
    mdiv :: f -> f -> f
    mrecip = mdiv mone
    mdiv x y = mmult x (mrecip y)
    {-# MINIMAL (mdiv | mrecip) #-}

instance Magma Integer where
    (<+>) = (+)

instance Semigroup Integer where
    (<>) = (+)

instance Monoid Integer where
    mempty = 0
    mappend = (+)

instance Group Integer where
    mnegate = negate
    mminus = (-)

instance Rung Integer where
    mmult = (*)

instance Ring Integer where
    mone = 1

instance Magma Float where
    (<+>) = (+)

instance Semigroup Float where
    (<>) = (+)

instance Monoid Float where
    mempty = 0
    mappend = (+)

instance Group Float where
    mnegate = negate
    mminus = (-)

instance Rung Float where
    mmult = (*)

instance Ring Float where
    mone = 1

instance Field Float where
    mrecip = recip
    mdiv = (/)

instance Magma Double where
    (<+>) = (+)

instance Semigroup Double where
    (<>) = (+)

instance Monoid Double where
    mempty = 0
    mappend = (+)

instance Group Double where
    mnegate = negate
    mminus = (-)

instance Rung Double where
    mmult = (*)

instance Ring Double where
    mone = 1

instance Field Double where
    mrecip = recip
    mdiv = (/)

newtype ApMono f a = AM {unAM :: f a}

deriving instance Functor f => Functor (ApMono f)
deriving instance Applicative f => Applicative (ApMono f)
deriving instance Monad f => Monad (ApMono f)

instance (Applicative f, Magma a) => Magma (ApMono f a) where
    (AM a) <+> (AM b) = AM (liftA2 (<+>) a b)

instance (Applicative f, Semigroup a) => Semigroup (ApMono f a) where
    (AM a) <> (AM b) = AM (liftA2 (<>) a b)

instance (Applicative f, Monoid a) => Monoid (ApMono f a) where
    mempty = AM (pure mempty)
    mappend (AM a) (AM b) = AM (liftA2 mappend a b)

instance (Applicative f, Group a) => Group (ApMono f a) where
    mnegate (AM a) = AM (fmap mnegate a)
    mminus (AM a) (AM b) = AM (liftA2 mminus a b)

instance (Applicative f, Rung a) => Rung (ApMono f a) where
    mmult (AM a) (AM b) = AM (liftA2 mmult a b)

instance (Applicative f, Ring a) => Ring (ApMono f a) where
    mone = AM (pure mone)

instance (Applicative f, Field a) => Field (ApMono f a) where
    mdiv (AM a) (AM b) = AM (liftA2 mdiv a b)
    mrecip (AM a) = AM (fmap mrecip a)