data Sequence a = a :- Sqc a

sqcFromList :: [a] -> a -> a
sqcFromList [] a = mkSqc (const a)
sqcFromList (x:xs) = x :- sqcFromList xs

mkSqc :: (a -> b) -> a -> Sequence b
mkSqc f a = f a :- mkSqc f (f a)