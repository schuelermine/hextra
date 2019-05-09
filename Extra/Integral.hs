module Extra.Integral where

toIntegral :: (Integral n, Integral m) => n -> m
toIntegral = fromInteger . toInteger
-- Converts any Integral to any other Integral

(+) :: (Integral i, Integral j) => i -> i -> j