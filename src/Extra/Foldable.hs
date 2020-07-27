module Extra.Foldable (maybeFoldr) where

import Control.Monad
import Extra.Function
import Data.Foldable

safeFoldr1 :: Foldable f => (a -> b -> b) -> f a -> Maybe b
safeFoldr1 = foldr (\a -> (f a <$>)) Nothing