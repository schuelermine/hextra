module Data.Nat.Compatible (subtract) where
-- Nat module that doesn't clash with Prelude
-- Only exports functions that don't clash,
-- does not export non-clashing aliases

import qualified Data.Nat as Nat

subtract = Nat.subtract