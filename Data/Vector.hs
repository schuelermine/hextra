{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoImplicitPrelude, TypeOperators #-}

module Data.Vector (Vector(Nil, Con), N(Z, S), toList) where
-- Defines Vector datatype and associated functions
-- Useful when you want to make sure things similar to (!!) or take always work

import Data.Nat as Nat
import Data.Nat.Kind as NatK

data Vector :: N -> * -> * where
    Nil :: Vector 'Z a
    Con :: a -> Vector n a -> Vector ('S n) a
-- Vector datatype, represents length-n linked lists
-- Takes a natural number to represent the length (from Data.Nat)
-- The empty list has a length of zero, and the cons operator adds one

toList :: Vector n a -> [a]
toList Nil        = []
toList (Con x xs) = x : toList xs
-- Turns a Vector into a list
-- Discards information about the length on the type level

append :: Vector n a -> Vector m a -> Vector (n + m) a
append Nil ys        = ys
append (Con x xs) ys = Con x (append xs ys)
-- Concatenates two Vectors
-- The resulting Vector's length is the sum of the original Vectors' lengths