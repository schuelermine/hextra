{-# LANGUAGE ExplicitForAll #-}

module Data.Sqc where
-- Defines the Sqc (pronounce: Sequence) type and related functions

import Extra.Tuple as Tup

data Sqc a = a :- Sqc a
-- Type for infinite lists
-- Pronounce: Sequence

sqcHead :: forall a. Sqc a -> a
sqcHead (x :- _) = x

sqcTail :: forall a. Sqc a -> Sqc a
sqcTail (_ :- xs) = xs
-- Like tail, but for Sqc
-- Safer, since a Sqc can't be empty

sqcFromList :: forall a. [a] -> a -> Sqc a
sqcFromList [] a = sqcUnfold dupe a
sqcFromList (x:xs) a = x :- sqcFromList xs a
-- Makes a Sqc from a finite list, repeats second argument when list is exhausted

sqcUnfold :: forall a b. (a -> (a, b)) -> a -> Sqc b
sqcUnfold f x =
    let (a, b) = f x
    in  b :- sqcUnfold f a
-- Like unfoldr, but for Sqc
-- Doesn't require the Maybe type in the signature,
-- since the list never ends

sqcCycle :: forall a. [a] -> Maybe (Sqc a)
sqcCycle [] = Nothing
sqcCycle l = Just $ sqcFromList (cycle l) undefined
-- Like cycle, but for Sqc
-- Safer, returns Nothing when the input is an empty list