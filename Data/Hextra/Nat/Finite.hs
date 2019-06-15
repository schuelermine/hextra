{-# LANGUAGE NoImplicitPrelude, ExplicitForAll, GADTs, DataKinds, TypeOperators #-}

module Data.Hextra.Nat.Finite where

import qualified Data.Hextra.Nat as N

data Finite n where
    FZ :: Finite n
    FS :: Finite n -> Finite ('N.S n)