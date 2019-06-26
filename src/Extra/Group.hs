module Extra.Group where

class Magma m where
    (<+>) :: m -> m -> m

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