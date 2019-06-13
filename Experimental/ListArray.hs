{-# LANGUAGE DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Experimental.ListArray where

import Data.Natural

data ListArray x where
    ListArray :: Natural -> (Natural -> Maybe x) -> ListArray x

getItem :: ListArray x -> Natural -> Maybe x
getItem (ListArray _ f) = f

getLength :: ListArray x -> Natural
getLength (ListArray n _) = n

append :: ListArray x -> ListArray x -> ListArray x
append (ListArray n f) (ListArray m g) = (ListArray o h) where
    o = n + m
    h p | p <= n = f p
        | p > n && p < o = g (p - n)