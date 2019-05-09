module Extra.Function (curry3, uncurry3, curry4, uncurry4, (<.), (.>)) where
-- Extra functions relating to functions (higher-order functions)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)
-- Like curry, but for functions with three arguments

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
-- Like uncurry, but for functions with three arguments

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f x y z w = f (x, y, z, w)
-- Like curry, but for functions with four arguments

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, w) = f x y z w
-- Like uncurry, but for functions with four arguments

infixr 9 <.
(<.) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(<.) f g x y = f (g x) (g y)
-- Applies a unary function on the inputs of a binary function

infixr 9 .>
(.>) :: (b -> c) -> (a -> a -> b) -> a -> a -> c
(.>) f g x y = f (g x y)
-- Applies a unary function on the output of a binary function