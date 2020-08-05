{-# LANGUAGE ExplicitForAll #-}

module Extra.Integral where

import Extra.Function
import Extra.Bifunctor

toIntegral :: forall i j. (Integral i, Integral j) => i -> j
toIntegral = fromInteger . toInteger
-- ^ Converts any Integral to any other Integral.

(\+) :: forall i j. (Integral i, Integral j) => i -> i -> j
(\+) = toIntegral .> (+)

(\-) :: forall i j. (Integral i, Integral j) => i -> i -> j
(\-) = toIntegral .> (-)

(\*) :: forall i j. (Integral i, Integral j) => i -> i -> j
(\*) = toIntegral .> (*)

genericQuot :: forall i j. (Integral i, Integral j) => i -> i -> j
genericQuot = toIntegral .> quot

genericRem :: forall i j. (Integral i, Integral j) => i -> i -> j
genericRem = toIntegral .> rem

genericQuotRem :: forall i j. (Integral i, Integral j) => i -> i -> (j, j)
genericQuotRem = bothmap toIntegral .> quotRem

genericDiv :: forall i j. (Integral i, Integral j) => i -> i -> j
genericDiv = toIntegral .> div

genericMod :: forall i j. (Integral i, Integral j) => i -> i -> j
genericMod = toIntegral .> mod

genericDivMod :: forall i j. (Integral i, Integral j) => i -> i -> (j, j)
genericDivMod = bothmap toIntegral .> divMod

safeQuot :: forall a. Integral a => a -> a -> Maybe a
safeQuot _ 0 = Nothing
safeQuot a b = Just $ quot a b

safeRem :: forall a. Integral a => a -> a -> Maybe a
safeRem _ 0 = Nothing
safeRem a b = Just $ rem a b

safeQuotRem :: forall a. Integral a => a -> a -> Maybe (a, a)
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

genericSafeQuot :: forall i j. (Integral i, Integral j) => i -> i -> Maybe j
genericSafeQuot _ 0 = Nothing
genericSafeQuot a b = Just $ genericQuot a b

genericSafeRem :: forall i j. (Integral i, Integral j) => i -> i -> Maybe j
genericSafeRem _ 0 = Nothing
genericSafeRem a b = Just $ genericRem a b

genericSafeQuotRem :: forall i j. (Integral i, Integral j) => i -> i -> Maybe (j, j)
genericSafeQuotRem _ 0 = Nothing
genericSafeQuotRem a b = Just $ genericQuotRem a b

genericSafeDiv :: forall i j. (Integral i, Integral j) => i -> i -> Maybe j
genericSafeDiv _ 0 = Nothing
genericSafeDiv a b = Just $ genericDiv a b

genericSafeMod :: forall i j. (Integral i, Integral j) => i -> i -> Maybe j
genericSafeMod _ 0 = Nothing
genericSafeMod a b = Just $ genericMod a b

genericSafeDivMod :: forall i j. (Integral i, Integral j) => i -> i -> Maybe (j, j)
genericSafeDivMod _ 0 = Nothing
genericSafeDivMod a b = Just $ genericDivMod a b