module Extra.Monad where

none :: m () -> m ()
none = id

forever :: Monad m => m a -> m a
forever m = m >> forever m