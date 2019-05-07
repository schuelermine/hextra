module Data.Nat.Instances where

import qualified Data.Nat.Internal as N

instance Num N.N where
    (+) = (N.+)
    (-) = (N.-)
    (*) = (N.*)
    fromInteger = N.fromInteger
    negate = id
    abs = id
    signum = const N.one

instance Eq N.N where
    N.Z     == N.Z     = True
    N.Z     == _       = False
    _       == N.Z     = False
    (N.S x) == (N.S y) = x == y

instance Ord N.N where
    compare  N.Z     N.Z    = EQ
    compare  N.Z     _      = LT
    compare  _       N.Z    = GT
    compare (N.S x) (N.S y) = compare x y

instance Enum N.N where
    succ = N.S
    pred N.Z = N.Z
    pred (N.S x) = x
    fromEnum = fromInteger . N.toInteger
    toEnum = fromIntegral

instance Real N.N where
    toRational = toRational . N.toInteger

instance Integral N.N where
    quotRem = N.quotRem
    divMod = N.quotRem
    toInteger = N.toInteger

instance Show N.N where
    showsPrec x = showsPrec x . N.toInteger
    show = show . N.toInteger
    showList = showList . map N.toInteger