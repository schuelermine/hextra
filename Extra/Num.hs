module Extra.Num (toIntegral, (<->)) where
-- TODO: Comment this file

import Extra.Function as Fun

toIntegral :: (Integral n, Integral m) => n -> m
toIntegral = fromInteger . toInteger
-- Allows you to convert any Integral to another Integral

(<->) :: (Num n) => n -> n -> n
(<->) = abs .> (-)
-- Absolute difference of Nums