{-# LANGUAGE TypeInType, DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Extra.Kind.Hetero where

import Extra.Kind.Constraint

type ClassContainer g f = g (InstanceOf f)