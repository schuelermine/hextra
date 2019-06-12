{-# LANGUAGE DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Experimental.VectorArray where

import Data.Finite
import Data.Singletons.TypeLits

data VectorArray n x where
    VectorArray :: SNat n -> (Finite n -> x) -> VectorArray n x

getItem :: VectorArray n x -> Finite n -> x
getItem (VectorArray _ f) = f
getLength :: VectorArray n x -> SNat n
getLength (VectorArray sn _) = sn