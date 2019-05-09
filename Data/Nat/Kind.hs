{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, NoImplicitPrelude, UndecidableInstances #-}

module Data.Nat.Kind where
-- Defines versions of (+), (*), (-) as type families (using DataKinds)
-- For details on definitions, see Data.Nat

import Data.Nat (N(Z, S))

type family (+) (a :: N) (b :: N) :: N
type instance 'Z + b     = b
type instance ('S a) + b = 'S (a + b)

type family (*) (a :: N) (b :: N) :: N
type instance 'Z * _     = 'Z
type instance ('S a) * b = (a * b) + b

type family (-) (a :: N) (b :: N) :: N
type instance 'Z - b          = b
type instance a - 'Z          = a
type instance ('S a) - ('S b) = a - b

type family Min (a :: N) (b :: N):: N
type instance Min 'Z _          = 'Z
type instance Min _ 'Z          = 'Z
type instance Min ('S a) ('S b) = 'S (Min a b)

type family Max (a :: N) (b :: N) :: N
type instance Max 'Z b          = b
type instance Max a 'Z          = a
type instance Max ('S a) ('S b) = 'S (Max a b)

-- See Data.Nat