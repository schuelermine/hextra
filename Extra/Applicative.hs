module Extra.Applicative where

zipA :: Applicative f => f a -> f b -> f (a, b)
zipA a b = (,) <$> a <*> b

unit :: Applicative f => f ()
unit = pure ()