{-# LANGUAGE ExplicitForAll #-}

-- | Implements safe versions of various functions.
-- Designed to be imported qualified, possibly with the alias Safe
module Extra.Safe where

-- ? Spinoff another module for functions which, instead of returning Nothing for empty lists, use NonEmpty

maximum :: forall a. Ord a => [a] -> Maybe a
maximum []     = Nothing
maximum (z:zs) = Just $ f z zs where
    f x []     = x
    f x (y:ys) = case compare x y of
        LT -> f x ys
        EQ -> f x ys
        GT -> f y ys

minimum :: forall a. Ord a => [a] -> Maybe a
minimum []      = Nothing
minimum (z:zs)  = Just $ f z zs where
    f x (y:ys)  = case compare x y of
        LT -> f y ys
        EQ -> f x ys
        GT -> f x ys
    f x []      = x

(!!?) :: forall a i. Integral i => [a] -> i -> Maybe a
[] !!? _     = Nothing
(x:_) !!? 0  = Just x
(_:xs) !!? n = xs !!? (n - 1)

safeCycle :: forall a. a -> [a] -> [a]
safeCycle a [] = repeat a
safeCycle a l  = a : l ++ safeCycle a l

safeQuotRem :: forall a. Integral a => a -> a -> Maybe a
safeQuotRem _ 0 = Nothing
safeQuotRem a b = Just $ quotRem a b

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