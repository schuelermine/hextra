{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoImplicitPrelude, TypeOperators, ExplicitForAll, PatternSynonyms #-}

module Data.Vector where
-- Defines Vector datatype and associated functions
-- Useful when you want to make sure things similar to (!!) or take always work

import Data.Nat as Nat
import Data.Nat.Kind as NatK
import qualified Prelude as P
import Data.Kind

data Vector :: N -> Type -> Type where
    Nil :: Vector 'Z a
    Con :: a -> Vector n a -> Vector ('S n) a
-- Vector datatype, represents length-n linked lists
-- Takes a natural number to represent the length (from Data.Nat)
-- The empty list has a length of zero, and the cons operator adds one

pattern VTrue <- Con _ _
pattern Vector1 a = Con a Nil
pattern Vector2 a b = Con a (Con b Nil)
pattern Vector3 a b c = Con a (Con b (Con c Nil))
pattern Vector4 a b c d = Con a (Con b (Con c (Con d Nil)))
pattern Vector5 a b c d e = Con a (Con b (Con c (Con d (Con e Nil))))
pattern Vector6 a b c d e f = Con a (Con b (Con c (Con d (Con e (Con f Nil)))))
pattern Vector7 a b c d e f g = Con a (Con b (Con c (Con d (Con e (Con f (Con g Nil))))))
pattern Vector8 a b c d e f g h = Con a (Con b (Con c (Con d (Con e (Con f (Con g (Con h Nil)))))))

toList :: forall a n. Vector n a -> [a]
toList Nil        = []
toList (Con x xs) = x : toList xs
-- Turns a Vector into a list
-- Discards information about the length on the type level

append :: forall a n m. Vector n a -> Vector m a -> Vector (n + m) a
append Nil ys        = ys
append (Con x xs) ys = Con x (append xs ys)
-- Concatenates two Vectors
-- The resulting Vector's length is the sum of the original Vectors' lengths

head :: forall a n. Vector ('S n) a -> a
head (Con x _) = x

last :: forall a n. Vector ('S n) a -> a
last (Con x Nil) = x
last (Con _ xs@VTrue) = last xs

tail :: forall a n. Vector ('S n) a -> Vector n a
tail (Con _ xs) = xs

init :: forall a n. Vector ('S n) a -> Vector n a
init (Con _ Nil) = Nil
init (Con x xs@VTrue) = Con x (init xs)

uncon :: forall a n. Vector ('S n) a -> (a, Vector n a)
uncon (Con x xs) = (x, xs)

null :: forall a n. Vector n a -> P.Bool
null Nil = P.True
null _ = P.False

vmap :: forall a b n. (a -> b) -> Vector n a -> Vector n b
vmap _ Nil = Nil
vmap f (Con x xs) = Con (f x) (vmap f xs)

plus :: forall a n. P.Num a => Vector n a -> Vector n a -> Vector n a
plus Nil Nil = Nil
plus (Con x xs) (Con y ys) = Con (x P.+ y) (plus xs ys)
-- Adds two Vectors of the same size
-- Equivalent to mathematical vector addition
-- Works for any vector size

cross :: forall a. P.Num a => Vector Three a -> Vector Three a -> Vector Three a
cross (Vector3 a b c) (Vector3 x y z) = Vector3 i j k where
    i = b P.* z P.- c P.* y
    j = c P.* x P.- a P.* z
    k = a P.* y P.- b P.* x

dot :: forall a n. P.Num a => Vector ('S n) a -> Vector ('S n) a -> a
dot (Con x Nil) (Con y Nil) = x P.* y
dot (Con x xs@VTrue) (Con y ys) = x P.* y P.+ dot xs ys

magnitude :: forall a n. P.Floating a => Vector n a -> a
magnitude v = P.sqrt P.$ f v where
    f :: forall a n. P.Num a => Vector n a -> a
    f Nil = 0
    f (Con x xs) = x P.^ 2 P.+ f xs

deriving instance P.Show a => P.Show (Vector n a)
deriving instance P.Ord a => P.Ord (Vector n a)
deriving instance P.Eq a => P.Eq (Vector n a)