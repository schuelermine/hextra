module Extra.Nat (replaceTail) where

import qualified Data.Nat as N

replaceTail :: N.N -> N.N -> N.N
replaceTail N.Z _     = N.Z
replaceTail (N.S _) x = N.S x
-- Experimental fuction
-- Replaces the tail of a natural number with another
-- Zero has no tail, and is left untouched
-- tail = value in the top-level constructor