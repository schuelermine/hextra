{-# LANGUAGE ExplicitForAll #-}

-- | Nat module that doesn't clash with Prelude
-- Only reexports functions that don't clash,
-- does not export auxiliary non-clashing aliases.
-- See Data.Nat.Internal for actual documentation.
module Data.Hextra.Nat (difference, Nat.N(Nat.Z, Nat.S)) where

import qualified Data.Hextra.Nat.Internal as Nat

difference :: forall n. Integral n => Nat.N -> Nat.N -> n
difference = Nat.difference
-- ^ Absolute difference of two natural numbers