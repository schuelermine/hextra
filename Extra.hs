{-# LANGUAGE NoMonomorphismRestriction, ExplicitForAll #-}

module Extra where

import Extra.Function

(%) :: forall i. Integral i => i -> i -> i
(%) = mod

iff a b c = if a then b else c

(#) = elem
($?) = uncurry3' iff
(!) = seq
(§) = uncurry
(<=>) = compare
(?+) = max
(?-) = min

readpromptfor :: Read a => (a -> IO b) -> IO b
readpromptfor f = (getLine >>= f . read)

promptfor :: (String -> IO b) -> IO b
promptfor = (getLine >>=)

wrapunwrap :: (a -> b, b -> a) -> (b -> b) -> a -> a
wrapunwrap (wrap, unwrap) f = unwrap . f . wrap

symmetrical :: (a -> b) -> (a -> b -> Bool) -> a -> Bool
symmetrical f g a = g a (f a)