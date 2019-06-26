{-# LANGUAGE DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Extra.Kind.Container where
-- TODO More commenting

data InstanceOf f where
    Of :: forall f a. f a => a -> InstanceOf f
    
instance Show (InstanceOf Show) where
    showsPrec i (Of a) = showsPrec i a
    show (Of a) = show a

type ClassContainer g f = g (InstanceOf f)

data Potentially a where
    Is :: forall a b. (b -> a) -> b -> Potentially a

getDefinite :: forall a. Potentially a -> a
getDefinite (Is f a) = f a

already :: forall a. a -> Potentially a
already = Is id

applying :: forall a b. a -> (a -> b) -> Potentially b
applying = flip Is

postpone :: forall a b. (a -> b) -> a -> Potentially b
postpone = Is