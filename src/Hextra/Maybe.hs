{-# LANGUAGE ExplicitForAll #-}

-- | Helpful extra functions concerning Maybe
module Hextra.Maybe where

fromNothing :: forall a. a -> Maybe a -> a
fromNothing x Nothing  = x
fromNothing _ (Just y) = y
-- ^ Extracts a value from a Maybe value,
-- uses replacement value in case of Nothing.

catchNothing :: forall a b. b -> (a -> Maybe b) -> a -> b
catchNothing x = (fromNothing x .)
-- ^ Composes fromNothing with a function.
-- Useful to make a function that relies on another function that returns a Maybe

equal :: Eq a => a -> a -> Maybe a
equal a b
    | a == b = Just a
    | otherwise = Nothing

could :: (a -> b -> Bool) -> a -> b -> Maybe b
could p a b
    | p a b = Just b
    | otherwise = Nothing