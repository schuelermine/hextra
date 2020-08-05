{-# LANGUAGE ExplicitForAll #-}

module Extra.Num where

import Extra.Function

(<->) :: forall n. (Num n) => n -> n -> n
(<->) = abs .> (-)
-- ^ Absolute difference of Nums.