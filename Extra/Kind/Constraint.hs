{-# LANGUAGE TypeInType, DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds, StandaloneDeriving #-}

module Extra.Kind.Constraint where

import Data.Kind

class Always a
instance Always a

data InstanceOf f where
    Of :: f a => a -> InstanceOf f

type family FoldConstraint (l :: [k -> Constraint]) (a :: k) :: Constraint
type instance FoldConstraint '[] _ = ()
type instance FoldConstraint (x ': xs) a = (x a, FoldConstraint xs a)

instance Show (InstanceOf Show) where
    showsPrec i (Of a) = showsPrec i a
    show (Of a) = show a