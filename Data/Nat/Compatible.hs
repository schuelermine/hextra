module Data.Nat.Compatible (difference) where
-- Nat module that doesn't clash with Prelude
-- Only reexports functions that don't clash,
-- does not export non-clashing aliases

import qualified Data.Nat as Nat

difference :: Integral n => Nat.N -> Nat.N -> n
difference = Nat.difference