{-# LANGUAGE ExplicitForAll #-}

module Extra.Integral where

import Extra.Function as Fun

toIntegral :: forall i j. (Integral i, Integral j) => i -> j
toIntegral = fromInteger . toInteger
-- ^ Converts any Integral to any other Integral.

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

safeQuot :: forall a. Integral a => a -> a -> Maybe (a, a)
safeQuot _ 0 = Nothing
safeQuot a b = Just $ quot a b

safeRem :: forall a. Integral a => a -> a -> Maybe a
safeRem _ 0 = Nothing
safeRem a b = Just $ rem a b

safeQuotRem :: forall a. Integral a => a -> a -> Maybe a
safeQuotRem _ 0 = Nothing
safeQuotRem a b = Just $ quotRem a b

safeDiv :: forall a. Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv a b = Just $ div a b

safeMod :: forall a. Integral a => a -> a -> Maybe a
safeMod _ 0 = Nothing
safeMod a b = Just $ mod a b

safeDivMod :: forall a. Integral a => a -> a -> Maybe (a, a)
safeDivMod _ 0 = Nothing
safeDivMod a b = Just $ divMod a b