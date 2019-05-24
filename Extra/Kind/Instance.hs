{-# LANGUAGE TypeInType, DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds, StandaloneDeriving #-}

module Extra.Kind.Instance where
-- TODO More commenting

import Data.Kind

class Always a
instance Always a

type family FoldConstraint (l :: [k -> Constraint]) (a :: k) :: Constraint
type instance FoldConstraint '[] _ = ()
type instance FoldConstraint (x ': xs) a = (x a, FoldConstraint xs a)