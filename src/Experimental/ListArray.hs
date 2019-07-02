{-# LANGUAGE DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Experimental.ListArray where

import Data.Hextra.Nat

data ListArray x where
    ListArray :: N -> (N -> Maybe x) -> ListArray x

getItem :: ListArray x -> N -> Maybe x
getItem (ListArray _ f) = f

getLength :: ListArray x -> N
getLength (ListArray n _) = n

append :: ListArray x -> ListArray x -> ListArray x
append (ListArray n f) (ListArray m g) = (ListArray o h) where
    o = n + m
    h p | p <= n = f p
        | p > n && p < o = g (p - n)