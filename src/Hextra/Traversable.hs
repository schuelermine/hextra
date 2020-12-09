{-# LANGUAGE ExplicitForAll #-}

module Hextra.Traversable where

import Hextra.Maybe
import Control.Applicative

findWhere :: (Foldable t, Functor t) => (a1 -> a2 -> Bool) -> t a2 -> a1 -> Maybe a2
findWhere p ys x = foldr (<|>) Nothing $ could p x <$> ys