module Extra.Maybe (catchNothing, fromNothing) where
-- Helpful extra functions concerning Maybe

fromNothing :: a -> Maybe a -> a
fromNothing x Nothing = x
fromNothing _ (Just y) = y
-- Extracts a value from a Maybe value,
-- uses replacement value in case of Nothing

catchNothing :: b -> (a -> Maybe b) -> a -> b
catchNothing x = (fromNothing x .)
-- Composes fromNothing with a function
-- Useful to make a function that relies on another function that returns a Maybe