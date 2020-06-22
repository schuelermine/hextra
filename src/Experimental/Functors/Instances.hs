{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, AllowAmbiguousTypes, ExplicitForAll, RankNTypes, TupleSections #-}

module Experimental.Functors.Instances where

import qualified Prelude as P
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
    (f:fs) <*> xs = map f xs P.++ (fs <*> xs)
    _ <:> [] = []
    [] <:> _ = []
    (x:xs) <:> ys = map (x,) ys P.++ (xs <:> ys)
    _ *> [] = []
    [] *> _ = []
    (_:xs) *> ys = ys P.++ (xs *> ys)
    _ <* [] = []
    [] <* _ = []
    (y:ys) <* xs = map (P.const y) xs P.++ (ys <* xs)

instance Apply [] where
    apply = (<*>)
    multiply = (<:>)
    andthen = (*>)
    after = (<*)

instance DecisiveUnfunctor [] where
    undecide (P.Left xs) = map P.Left xs
    undecide (P.Right xs) = map P.Right xs

instance Monad [] where
    return x = [x]
    join [] = []
    join (x:xs) = x P.++ join xs
    [] >>= _ = []
    (x:xs) >>= f = f x P.++ (xs >>= f)