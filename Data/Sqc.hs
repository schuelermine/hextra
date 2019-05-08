module Data.Sqc where

import Extra.Tuple (dupe) as Tup

data Sqc a = a :- Sqc a

fromList :: [a] -> a -> Sqc a
fromList [] a = unfoldr dupe a
fromList (x:xs) a = x :- fromList xs a

unfoldr :: (a -> (a, b)) -> a -> Sqc b
unfoldr f x =
    let (a, b) = f x
    in  b :- unfoldr f a