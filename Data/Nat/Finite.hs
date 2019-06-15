{-# LANGUAGE NoImplicitPrelude, ExplicitForAll, GADTs, DataKinds, TypeOperators #-}

module Data.Nat.Finite where

import qualified Data.Nat as N

data Finite n where
    FZ :: Finite n
    FS :: Finite n -> Finite ('N.S n)