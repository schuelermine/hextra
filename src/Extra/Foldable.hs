{-# LANGUAGE ExplicitForAll #-}

module Extra.Foldable (safeFoldr1) where

import Control.Monad
import Extra.Function
import Data.Foldable

safeFoldr1 :: forall f a. Foldable f => (a -> a -> a) -> f a -> Maybe a
safeFoldr1 f = foldr q Nothing where
    q x Nothing = Just x
    q x (Just y) = Just $ f x y

safeMaximum :: forall f a. (Foldable f, Ord a) => f a -> Maybe a
safeMaximum = safeFoldr1 max
safeMinimum :: forall f a. (Foldable f, Ord a) => f a -> Maybe a
safeMinimum = safeFoldr1 min