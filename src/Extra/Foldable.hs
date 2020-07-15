module Extra.Foldable (maybeFoldr) where

import Control.Monad
import Extra.Function

newtype MonoidPlus f a = MonoidPlus f a

instance MonadPlus f => Monoid (MonoidPlus f) where
    mempty = MonoidPlus mzero
    mappend = MonoidPlus .> mappend

safeFoldr1 :: Foldable f => (a -> b -> b) -> f a -> Maybe b
safeFoldr1 = 