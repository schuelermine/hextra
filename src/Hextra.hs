{-# LANGUAGE ExplicitForAll #-}

g :: forall a. (a -> Bool) -> a -> Maybe (a -> Bool)
g f x
    | f x = Just f
    | otherwise = Nothing

f :: forall a b. (a -> b -> Bool) -> [a] -> b -> Maybe a
f _ [] _ = Nothing
f (<?>) (x:xs) y
    | x <?> y = Just x
    | otherwise = f (<?>) xs y