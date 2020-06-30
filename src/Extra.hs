{-# LANGUAGE NoMonomorphismRestriction, ExplicitForAll #-}

module Extra where

import Extra.Function

(%) :: forall i. Integral i => i -> i -> i
(%) = mod

if' a b c = if a then b else c

(#) = elem
($?) = uncurry3' if'
(!) = seq
(§) = uncurry
(<=>) = compare
(?+) = max
(?-) = min

wrapunwrap :: (a -> b, b -> a) -> (b -> b) -> a -> a
wrapunwrap (wrap, unwrap) f = unwrap . f . wrap

symmetrical :: (a -> b) -> (a -> b -> Bool) -> a -> Bool
symmetrical f g a = g a (f a)

replace :: Eq a => a -> a -> a -> a
replace a b = applyIf (== a) (const b)

applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf p f a
    | p a = f a
    | True = a