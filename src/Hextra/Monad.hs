{-# LANGUAGE ExplicitForAll #-}

module Hextra.Monad where

none :: forall m. m () -> m ()
none = id

forever :: forall m a. Monad m => m a -> m a
forever m = m >> forever m
