module Data.Sqc where

import Extra.Tuple

data Sqc a = a :- Sqc a

sqcFromList :: [a] -> a -> Sqc a
sqcFromList [] a = unfoldSqc dupe a
sqcFromList (x:xs) a = x :- sqcFromList xs a

unfoldSqc :: (a -> (a, b)) -> a -> Sqc b
unfoldSqc f x =
    let (a, b) = f x
    in  b :- unfoldSqc f a

main = return ()