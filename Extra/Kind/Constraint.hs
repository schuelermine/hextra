{-# LANGUAGE TypeInType, DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Extra.Kind.Constraint where

import Data.Kind

class Always a
instance Always a

data InstanceOf f where
    C :: f a => a -> InstanceOf f

type family FoldConstraint (l :: [k -> Constraint]) (a :: k) :: Constraint
type instance FoldConstraint '[] _ = ()
type instance FoldConstraint (x ': xs) a = (x a, FoldConstraint xs a)