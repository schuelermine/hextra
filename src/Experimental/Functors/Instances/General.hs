{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, AllowAmbiguousTypes, ExplicitForAll, RankNTypes, TupleSections #-}

module Experimental.Functors.Instances.General where

import qualified Prelude as P
import Experimental.Functors

instance P.Functor f => Functor f where
    fmap = P.fmap

instance P.Applicative => ApplicativeFunctor f where
    (<*>) = (P.<*>)
    pure = P.pure

instance P.Monad => Monad f where
