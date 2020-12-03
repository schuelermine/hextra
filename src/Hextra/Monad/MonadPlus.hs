module Hextra.Monad.MonadPlus where

import Control.Monad

newtype MonoidPlus f a = MonoidPlus (f a)

instance MonadPlus f => Semigroup (MonoidPlus f a) where
    (MonoidPlus x) <> (MonoidPlus y) = MonoidPlus $ mplus x y

instance MonadPlus f => Monoid (MonoidPlus f a) where
    mempty = MonoidPlus mzero
    mappend (MonoidPlus x) (MonoidPlus y) = MonoidPlus $ mplus x y
