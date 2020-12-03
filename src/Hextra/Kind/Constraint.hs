{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, PolyKinds, TypeOperators, FlexibleInstances #-}

module Hextra.Kind.Constraint where
-- TODO More commenting

import Data.Kind

class Always a
instance Always a

type family FoldConstraint (l :: [k -> Constraint]) (a :: k) :: Constraint
type instance FoldConstraint '[] _ = ()
type instance FoldConstraint (x ': xs) a = (x a, FoldConstraint xs a)
