{-# LANGUAGE QuantifiedConstraints, StandaloneDeriving, ExistentialQuantification, RankNTypes, UndecidableInstances, FlexibleInstances #-}

module Data.Tree where
-- Defines Tree types

-- Various tree types:

data Tree f a = Knot a (f (Tree f a)) | End
data Skeleton f = Joint (f (Skeleton f)) | Condyle
data NonEmptyTree f a = Node a (f (NonEmptyTree f a)) | Leaf a
data Flower f a = Stems (f (Flower f a)) | Blossom a
data Reed f = Fork (f (Reed f))
data Bush f a = Split a (f (Bush f a))

data Tree2 a = Knot2 a (Tree2 a) (Tree2 a) | End2
data Skeleton2 = Joint2 Skeleton2 Skeleton2 | Condyle2
data NonEmptyTree2 a = Node2 a (NonEmptyTree2 a) (NonEmptyTree2 a) | Leaf2 a
data Flower2 a = Stems2 (Flower2 a) (Flower2 a) | Blossom2 a

data Tree3 a = Knot3 a (Tree3 a) (Tree3 a) (Tree3 a) | End3
data Skeleton3 = Joint3 Skeleton3 Skeleton3 Skeleton3 | Condyle3
data NonEmptyTree3 a = Node3 a (NonEmptyTree3 a) (NonEmptyTree3 a) (NonEmptyTree3 a) | Leaf3 a
data Flower3 a = Stems3 (Flower3 a) (Flower3 a) (Flower3 a) | Blossom3 a

deriving instance (Show a, forall x. Show x => Show (f x)) => Show (Tree f a)
deriving instance (forall x. Show x => Show (f x)) => Show (Skeleton f)
deriving instance (Show a, forall x. Show x => Show (f x)) => Show (NonEmptyTree f a)
deriving instance (Show a, forall x. Show x => Show (f x)) => Show (Flower f a)

deriving instance (Read a, forall x. Read x => Read (f x)) => Read (Tree f a)
deriving instance (forall x. Read x => Read (f x)) => Read (Skeleton f)
deriving instance (Read a, forall x. Read x => Read (f x)) => Read (NonEmptyTree f a)
deriving instance (Read a, forall x. Read x => Read (f x)) => Read (Flower f a)

deriving instance (Eq a, forall x. Eq x => Eq (f x)) => Eq (Tree f a)
deriving instance (forall x. Eq x => Eq (f x)) => Eq (Skeleton f)
deriving instance (Eq a, forall x. Eq x => Eq (f x)) => Eq (NonEmptyTree f a)
deriving instance (Eq a, forall x. Eq x => Eq (f x)) => Eq (Flower f a)

--deriving instance (Ord a, forall x. Ord x => Ord (f x)) => Ord (Tree f a)
--deriving instance (forall x. Ord x => Ord (f x)) => Ord (Skeleton f)
--deriving instance (Ord a, forall x. Ord x => Ord (f x)) => Ord (NonEmptyTree f a)
--deriving instance (Ord a, forall x. Ord x => Ord (f x)) => Ord (Flower f a)