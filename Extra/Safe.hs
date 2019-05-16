{-# LANUAGE ExplicitForAll #-}

module Extra.Safe where
-- Implements safe versions of various functions
-- Designed to be imported qualified, possibly with the alias Safe
-- ? Spinoff another module for functions which, instead of returning Nothing for empty lists, use NonEmpty

head :: forall a. [a] -> Maybe a
head []    = Nothing
head (x:_) = Just x

tail :: forall a. [a] -> Maybe [a]
tail []     = Nothing
tail (_:xs) = Just xs

last :: forall a. [a] -> Maybe a
last []     = Nothing
last (x:[]) = Just x
last (_:xs) = Extra.Safe.last xs

init :: forall a. [a] -> Maybe [a]
init []     = Nothing
init (_:[]) = Just []
init (x:xs) = (x :) <$> Extra.Safe.init xs

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

(!!) :: forall a i. Integral i => [a] -> i -> Maybe a
[] !! _     = Nothing
(x:_) !! 0  = Just x
(_:xs) !! n = xs Extra.Safe.!! (n - 1)

cycle :: forall a. a -> [a] -> [a]
cycle a [] = repeat a
cycle a l  = l ++ Extra.Safe.cycle a l

quot :: forall a. Integral a => a -> a -> Maybe a
quot _ 0 = Nothing
quot a b = Just $ Prelude.quot a b

rem :: forall a. Integral a => a -> a -> Maybe a
rem _ 0 = Nothing
rem a b = Just $ Prelude.rem a b

quotRem :: forall a. Integral a => a -> a -> Maybe (a, a)
quotRem _ 0 = Nothing
quotRem a b = Just $ Prelude.quotRem a b

div :: forall a. Integral a => a -> a -> Maybe a
div _ 0 = Nothing
div a b = Just $ Prelude.div a b

mod :: forall a. Integral a => a -> a -> Maybe a
mod _ 0 = Nothing
mod a b = Just $ Prelude.mod a b

divMod :: forall a. Integral a => a -> a -> Maybe (a, a)
divMod _ 0 = Nothing
divMod a b = Just $ Prelude.divMod a b