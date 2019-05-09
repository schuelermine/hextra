module Data.Sqc where
-- Defines the Sqc (pronounce: Sequence) type and related functions

import Extra.Tuple as Tup
import Extra.List as List

data Sqc a = a :- Sqc a
-- Type for infinite lists
-- Pronounce: Sequence

sqcFromList :: [a] -> a -> Sqc a
sqcFromList [] a = unfoldSqc dupe a
sqcFromList (x:xs) a = x :- sqcFromList xs a
-- Makes a Sqc from a finite list, repeats second argument when list is exhausted

sqcUnfold :: (a -> (a, b)) -> a -> Sqc b
sqcUnfold f x =
    let (a, b) = f x
    in  b :- unfoldSqc f a
-- Like unfoldr, but for Sqc
-- Doesn't require the Maybe type in the signature,
-- since the list never ends.

sqcCycle :: [a] -> Maybe (Sqc a)
sqcCycle [] = Nothing
sqcCycle l = scqFromList (cycle l) undefined
-- Like cycle, but for Sqc
-- Safer, returns Nothing when the input is an empty list