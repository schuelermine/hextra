{-# LANGUAGE ExplicitForAll #-}

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

applyIfJustMonoid :: forall a b x. Monoid b => (a -> b) -> (x -> a) -> Maybe x -> b
applyIfJustMonoid f g m = case m of
    Just x -> g (f x)
    Nothing -> mempty

combineIfJust :: forall a b x. (a -> b -> b) -> (x -> a) -> Maybe x -> b -> b
combineIfJust g f m b = case m of
    Just x -> g (f x) b
    Nothing -> b
-- Combines two things where one of them is wrapped in Maybe
-- If the Maybe thing is Nothing, just return the plain thing
-- Requires that 2nd argument type and output type of the combining function are the same

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
-- Flipped (.>)

infixr 9 >.
(>.) :: forall a b c. (a -> b) -> (b -> b -> c) -> a -> a -> c
(>.) = flip (<.)
-- Flipped (<.)

infixr 9 ?
(?) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
(?) = flip (.)
-- Flipped (.)