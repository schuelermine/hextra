{-# LANGUAGE ViewPatterns #-}

module Experimental.Array (Array(), fromFunction, fromTotalFunction, getItem, set, remove, fromList, getItems, slice, genericGetItems, mono, empty) where

import Data.Hextra.Nat
import qualified Control.Applicative as A

newtype Array x = Array { getItem :: N -> Maybe x }

(!!?) :: [x] -> N -> Maybe x
(x:_) !!? (Z) = Just x
(_:xs) !!? (S n) = xs !!? n
_ !!? _ = Nothing

fromFunction :: (N -> Maybe x) -> Array x
fromFunction = Array

fromTotalFunction :: (N -> x) -> Array x
fromTotalFunction = Array . (Just .)

fromList :: [x] -> Array x
fromList xs = Array (xs !!?)

set :: N -> x -> Array x -> Array x
set n x a = Array $ \m ->
    case n == m of
        True -> Just x
        False -> getItem a m

remove :: N -> Array x -> Array x
remove n a = Array $ \m ->
    case n == m of
        True -> Nothing
        False -> getItem a m

getItems :: [N] -> Array x -> [Maybe x]
getItems ns array = (getItem array) <$> ns

slice :: N -> N -> Array x -> [Maybe x]
slice n m = getItems [n..m]

genericGetItems :: Functor f => f N -> Array x -> f (Maybe x)
genericGetItems ns a = (getItem a) <$> ns

mono :: x -> Array x
mono x = Array . const $ Just x

empty :: Array x
empty = A.empty

instance Functor Array where
    fmap f a = Array $ \n ->
        f <$> (getItem a n)

instance Applicative Array where
    pure x = Array . const $ Just x
    f <*> a = Array $ \n ->
        ($) <$> getItem f n <*> getItem a n

instance A.Alternative Array where
    empty = Array $ const Nothing
    a <|> b = Array $ \n ->
        case getItem a n of
            Nothing -> getItem b n
            x -> x