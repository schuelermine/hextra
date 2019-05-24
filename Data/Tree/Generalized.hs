{-# LANGUAGE QuantifiedConstraints, StandaloneDeriving, ExistentialQuantification, RankNTypes, UndecidableInstances, FlexibleInstances, ExplicitForAll #-}

module Data.Tree.Generalized where
    
import Data.Bifunctor

data XTree f a = XNode (f a (XTree f a))
-- Extremely general tree type
-- Generalizes all previously listed examples,
-- though cumbersome to use
-- For example, type Bush [] = XTree G where data G x y = G x [y]
-- Split 1 [] = XNode (G 1 [])
-- Split 1 [Split 2 [], Split 2 [], Split 2 []] =
-- XNode (G 1 [XNode (G 2 []), XNode (G 2 []), XNode (G 2 [])])

unXNode :: forall f a. XTree f a -> f a (XTree f a)
unXNode (XNode f) = f

data YTree f g a = YNode (f a (g (YTree f g a)))
-- Slightly less general tree type
-- Much more useful in general, though

unYNode :: forall f g a. YTree f g a -> f a (g (YTree f g a))
unYNode (YNode f) = f
-- Unwraps a YTree's YNode

instance (Bifunctor f, Functor g) => Functor (YTree f g) where
    fmap f (YNode m) = YNode $ bimap f (fmap (fmap f)) m

deriving instance 
    ( Show a
    , forall x y. (Show x, Show y) => Show (f x y)
    , forall z. Show z => Show (g z)
    ) => Show (YTree f g a)
deriving instance 
    ( Read a
    , forall x y. (Read x, Read y) => Read (f x y), forall z. Read z => Read (g z)) => Read (YTree f g a)
deriving instance
    ( Eq a
    , forall x y. (Eq x, Eq y) => Eq (f x y)
    , forall z. Eq z => Eq (g z)
    ) => Eq (YTree f g a)
deriving instance
    ( Ord a
    , forall x y. (Ord x, Ord y) => Ord (f x y)
    , forall z. Ord z => Ord (g z)
    , Eq  a
    , forall x y. (Eq  x, Eq  y) => Eq  (f x y)
    , forall z. Eq  z => Eq  (g z)
    ) => Ord (YTree f g a)
    -- This instance is very weird due to a bug in GHC 8.6.5,
    -- see https://stackoverflow.com/questions/56192019/is-it-possible-to-derive-an-implementation-for-ord-for-this-tree-type-and-if
    -- for more info