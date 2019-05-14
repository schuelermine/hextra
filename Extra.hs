{-# LANGUAGE NoMonomorphismRestriction #-}

module Extra where

import Extra.Function

(%) :: Integral i => i -> i -> i
(%) = mod

iff a b c = if a then b else c

(#) = elem
(?) = uncurry3' iff
(!) = seq
(§) = uncurry
(<=>) = compare
(?+) = max
(?-) = min