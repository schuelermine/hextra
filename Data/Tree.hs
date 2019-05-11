module Data.Tree where
-- Defines Tree types

data Tree f a = Split a (f (Tree f a)) | End
data Skeleton f = Joint (f (Skeleton f)) | Condyle
data NonEmptyTree f a = Node a (f (NonEmptyTree f a)) | Leaf a
data Flower f a = Branch (f (Flower f a)) | Blossom a