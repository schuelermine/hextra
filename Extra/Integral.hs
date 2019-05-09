module Extra.Integral where

import Extra.Function as Fun

toIntegral :: (Integral i, Integral j) => i -> j
toIntegral = fromInteger . toInteger
-- Converts any Integral to any other Integral

plus :: (Integral i, Integral j) => i -> i -> j
plus = toIntegral .> (+)

minus :: (Integral i, Integral j) => i -> i -> j
minus = toIntegral .> (-)

times :: (Integral i, Integral j) => i -> i -> j
times = toIntegral .> (*)

quotient :: (Integral i, Integral j) => i -> i -> j
quotient = toIntegral .> quot

remainder :: (Integral i, Integral j) => i -> i -> j
remainder = toIntegral .> rem

division :: (Integral i, Integral j) => i -> i -> j
division = toIntegral .> div

modulo :: (Integral i, Integral j) => i -> i -> j
modulo = toIntegral .> mod