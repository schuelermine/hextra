{-# LANGUAGE ExplicitForAll #-}

module Data.Stream where
-- Defines the Stream (pronounce: Sequence) type and related functions

import Extra.Tuple as Tup

data Stream a = a :- Stream a
-- Type for infinite lists
-- Pronounce: Sequence

streamHead :: forall a. Stream a -> a
streamHead (x :- _) = x

streamTail :: forall a. Stream a -> Stream a
streamTail (_ :- xs) = xs
-- Like tail, but for Stream
-- Safer, since a Stream can't be empty

streamFromList :: forall a. [a] -> a -> Stream a
streamFromList [] a = streamUnfold dupe a
streamFromList (x:xs) a = x :- streamFromList xs a
-- Makes a Stream from a finite list, repeats second argument when list is exhausted

streamUnfold :: forall a b. (a -> (a, b)) -> a -> Stream b
streamUnfold f x =
    let (a, b) = f x
    in  b :- streamUnfold f a
-- Like unfoldr, but for Stream
-- Doesn't require the Maybe type in the signature,
-- since the list never ends
-- No distinction between right and left unfolds,
-- since you can't build Streams from the right

streamCycle :: forall a. [a] -> Maybe (Stream a)
streamCycle [] = Nothing
streamCycle l = Just $ streamFromList (cycle l) undefined
-- Like cycle, but for Stream
-- Safer, returns Nothing when the input is an empty list