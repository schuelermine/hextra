module Extra.Num (toIntegral) where
-- TODO: Comment this file

toIntegral :: (Integral n, Integral m) => n -> m
toIntegral = fromInteger . toInteger

(<->) :: (Num n) => n -> n -> n
(<->) = abs .> (-)