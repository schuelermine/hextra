{-# LANGUAGE TypeInType, DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Extra.Kind.Hetero where

data InstanceOf f where
    Of :: forall f a. f a => a -> InstanceOf f
    
instance Show (InstanceOf Show) where
    showsPrec i (Of a) = showsPrec i a
    show (Of a) = show a

type ClassContainer g f = g (InstanceOf f)

data Potentially a where
    Is :: forall a b. (b -> a) -> b -> Potentially a

getDefinite :: Potentially a -> a
getDefinite (Is f a) = f a

already :: a -> Potentially a
already = Is id

applying :: a -> (a -> b) -> Potentially b
applying = flip Is

postpone :: (a -> b) -> a -> Potentially b
postpone = Is