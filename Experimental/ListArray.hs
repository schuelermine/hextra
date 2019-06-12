{-# LANGUAGE DataKinds, ConstraintKinds, GADTs, TypeFamilies, MultiParamTypeClasses, ExplicitForAll, FlexibleInstances, UndecidableInstances, QuantifiedConstraints, RankNTypes, TypeOperators, FunctionalDependencies, ScopedTypeVariables, KindSignatures, NoStarIsType, PolyKinds #-}

module Experimental.ListArray where

import Data.Natural

data ListArray x where
    ListArray :: Natural -> x -> (Natural -> x) -> ListArray x

getFallback :: ListArray x -> x
getFallback (ListArray _ x _) = x

getItem :: ListArray x -> Natural -> x
getItem (ListArray _ _ f) = f

getLength :: ListArray x -> Natural
getLength (ListArray n _ _) = n

append (ListArray n x f) (ListArray m _ g) = (ListArray o z h) where
    o = n + m
    z = x
    h p | p <= n = f p
        | p > n = g (p - n)
        | otherwise = z