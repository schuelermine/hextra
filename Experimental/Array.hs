{-# LANGUAGE ViewPatterns #-}

module Experimental.Array (Array(), fromFunction, fromTotalFunction, getItem, set, remove, fromList, getItems, slice, zipArray, supplement, genericGetItems, empty, mono) where

import Data.Natural

newtype Array x = Array { getItem :: Natural -> Maybe x }

(!!?) :: [x] -> Natural -> Maybe x
(x:_) !!? (view -> Zero) = Just x
(_:xs) !!? (view -> Succ n) = xs !!? n
_ !!? _ = Nothing

fromFunction :: (Natural -> Maybe x) -> Array x
fromFunction = Array

fromTotalFunction :: (Natural -> x) -> Array x
fromTotalFunction = Array . (Just .)

set :: Natural -> x -> Array x -> Array x
set n x a = Array $ \m ->
    case n == m of
        True -> Just x
        False -> getItem a m

remove :: Natural -> Array x -> Array x
remove n a = Array $ \m ->
    case n == m of
        True -> Nothing
        False -> getItem a m

instance Functor Array where
    fmap f a = Array $ \n ->
        f <$> (getItem a n)

arrayApply :: Array (x -> y) -> Array x -> Array y
arrayApply f a = Array $ \n ->
    ($) <$> getItem f n <*> getItem a n

instance Applicative Array where
    pure = mono
    (<*>) = arrayApply

fromList :: [x] -> Array x
fromList xs = Array (xs !!?)

getItems :: [Natural] -> Array x -> [Maybe x]
getItems ns array = (getItem array) <$> ns

slice :: Natural -> Natural -> Array x -> [Maybe x]
slice n m = getItems [n..m]

zipArray :: Array x -> Array y -> Array (x, y)
zipArray a b = (,) <$> a <*> b

supplement :: Array x -> Array x -> Array x
supplement a b = Array $ \n ->
    case getItem a n of
        Nothing -> getItem b n
        x -> x

genericGetItems :: Functor f => f Natural -> Array x -> f (Maybe x)
genericGetItems ns a = (getItem a) <$> ns

empty :: Array x
empty = Array $ const Nothing

mono :: x -> Array x
mono x = Array . const $ Just x

instance Show x => Show (Array x) where
    show a = show $ getItems [0..] a