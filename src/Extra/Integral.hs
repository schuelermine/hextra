{-# LANGUAGE ExplicitForAll #-}

module Extra.Integral where

import Extra.Function as Fun

toIntegral :: forall i j. (Integral i, Integral j) => i -> j
toIntegral = fromInteger . toInteger
-- Converts any Integral to any other Integral.

plus :: forall i j. (Integral i, Integral j) => i -> i -> j
plus = toIntegral .> (+)

minus :: forall i j. (Integral i, Integral j) => i -> i -> j
minus = toIntegral .> (-)

times :: forall i j. (Integral i, Integral j) => i -> i -> j
times = toIntegral .> (*)

quotient :: forall i j. (Integral i, Integral j) => i -> i -> j
quotient = toIntegral .> quot

remainder :: forall i j. (Integral i, Integral j) => i -> i -> j
remainder = toIntegral .> rem

division :: forall i j. (Integral i, Integral j) => i -> i -> j
division = toIntegral .> div

modulo :: forall i j. (Integral i, Integral j) => i -> i -> j
modulo = toIntegral .> mod