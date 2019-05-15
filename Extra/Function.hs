{-# LANUAGE ExplicitForall #-}

module Extra.Function where
-- Extra functions relating to functions (higher-order functions)


curry3 :: forall a b c d. ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)
-- Like curry, but for functions with three arguments

uncurry3 :: forall a b c d. (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
-- Like uncurry, but for functions with three arguments

uncurry3' :: forall a b c d. (a -> b -> c -> d) -> a -> (b, c) -> d
uncurry3' f a (b, c) = f a b c

curry4 :: forall a b c d e. ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f x y z w = f (x, y, z, w)
-- Like curry, but for functions with four arguments

uncurry4 :: forall a b c d e. (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (x, y, z, w) = f x y z w
-- Like uncurry, but for functions with four arguments

infixr 9 <.
(<.) :: forall b c a. (b -> b -> c) -> (a -> b) -> a -> a -> c
(<.) f g x y = f (g x) (g y)
-- Applies a unary function on the inputs of a binary function

infixr 9 .>
(.>) :: forall b c a. (b -> c) -> (a -> a -> b) -> a -> a -> c
(.>) f g x y = f (g x y)
-- Applies a unary function on the output of a binary function

infixr 9 .<
(.<) :: forall a b c. (a -> a -> b) -> (b -> c) -> a -> a -> c
(.<) = flip (.>)

infixr 9 >.
(>.) :: forall a b c. (a -> b) -> (b -> b -> c) -> a -> a -> c
(>.) = flip (<.)

infixr 9 >>>>
(>>>>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
(>>>>) = flip (.)