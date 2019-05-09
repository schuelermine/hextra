module Extra.Num where

import Extra.Function as Fun

(<->) :: (Num n) => n -> n -> n
(<->) = abs .> (-)
-- Absolute difference of Nums