module Extra.Foldable (maybeFoldr) where

import Control.Monad
import Extra.Function
import Data.Foldable

safeFoldr1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
safeFoldr1 f = foldr q Nothing where
    q x Nothing = Just x
    q x (Just y) = Just $ f x y

safeMaximum = safeFoldr1 max
safeMinimum = safeFoldr1 min