{-# LANGUAGE NoMonomorphismRestriction, ExplicitForAll #-}

module Extra where

import Extra.Function

(%) :: forall i. Integral i => i -> i -> i
(%) = mod

iff a b c = if a then b else c

(#) = elem
(?) = uncurry3' iff
(!) = seq
(§) = uncurry
(<=>) = compare
(?+) = max
(?-) = min

readpromptfor :: Read a => (a -> IO b) -> IO b
readpromptfor f = (getLine >>= f . read)

promptfor :: (String -> IO b) -> IO b
promptfor = (getLine >>=)