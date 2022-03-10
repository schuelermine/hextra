foldApp :: (c -> b -> c) -> [a -> b] -> c -> a -> c
foldApp g fs x y = foldl g x (map ($ y) fs)
