{-# LANGUAGE ExplicitForAll #-}

module Hextra.Num where

import Hextra.Function

(<->) :: forall n. (Num n) => n -> n -> n
(<->) = abs .> (-)
-- ^ Absolute difference of Nums.