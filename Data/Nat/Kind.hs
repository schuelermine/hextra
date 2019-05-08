{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, NoImplicitPrelude, UndecidableInstances #-}

module Data.Nat.Kind (type (+), type (*), type (-)) where
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

type family min (a :: N) (b :: N) :: N
type instance min 'Z _          = 'Z
type instance min _ 'Z          = a
type instance min ('S a) ('S b) = S (min a b)

type family max (a :: N) (b :: N) :: N
type instance max 'Z b          = b
type instance max a 'Z          = a
type instance max ('S a) ('S b) = S (max a b)

-- See Data.Nat