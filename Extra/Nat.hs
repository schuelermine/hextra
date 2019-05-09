module Extra.Nat (replaceTail) where

import qualified Data.Nat as N

replaceTail :: N.N -> N.N -> N.N
replaceTail N.Z _     = N.Z
replaceTail (N.S _) x = N.S x