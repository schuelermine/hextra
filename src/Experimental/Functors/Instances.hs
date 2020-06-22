{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, AllowAmbiguousTypes, ExplicitForAll, RankNTypes, TupleSections #-}

module Experimental.Functors.Instances where

import Prelude as P
import Experimental.Functors

instance Functor [] where
    map _ [] = []
    map f (x:xs) = f x : map f xs

instance InvariantFunctor [] where
    invmap f _ = map f

instance ApplicativeFunctor [] where
    pure x = [x]
    unit = [()]
    _ <*> [] = []
    [] <*> _ = []
    (f:fs) <*> xs = map f xs ++ (fs <*> xs)
    _ <:> [] = []
    [] <:> _ = []
    (x:xs) <:> ys = map (x,) ys ++ (xs <:> ys)
    _ *> [] = []
    [] *> _ = []
    (_:xs) *> ys = ys ++ (xs *> ys)
    _ <* [] = []
    [] <* _ = []
    (y:ys) <* xs = map (const y) xs ++ (ys <* xs)

instance Apply [] where
    apply = (<*>)
    multiply = (<:>)
    andthen = (*>)
    after = (<*)

instance DecisiveUnfunctor [] where
    undecide (Left xs) = map Left xs
    undecide (Right xs) = map Right xs

instance Monad [] where
    return x = [x]
    join [] = []
    join (x:xs) = x ++ join xs
    [] >>= _ = []
    (x:xs) >>= f = f x ++ (xs >>= f)