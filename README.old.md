## My personal Haskell repository

### Contents of this README

**TODO:** *Rearrange entries to reflect alphabetical order*

* Contents
* Purpose
* File info
  * Data
    * Cirq.hs
    * Cirq
      * Basic.hs
      * Utils.hs
      * Tools.hs
    * Nat.hs
    * Nat
      * Internal.hs
      * Kind.hs
    * Tree.hs
    * Tree
      * Generalized.hs
    * Sqc.hs
    * Vector.hs
  * Extra.hs
  * Extra
    * Tuple.hs
    * Bifunctor.hs
    * CirqUnpacked.hs
    * Function.hs
    * Maybe.hs
    * Safe.hs
  * Deprecated.hs

### Purpose

I use this to write any code that's not for a project that already has a folder.
Most projects, actually, are stored inside this folder, since it helps me to avoid duplicate module names.

### Explaining files

#### Data/Cirq.hs

`Data.Cirq`

Exports all functions defined in the modules in the `Cirq` *folder* as a neat, complete package.

Don't import alongside the other Cirq modules.
No reason for that, it's just bad style and bad practice.

#### Data/Cirq/Base.hs

`Data.Cirq.Base`

Defines the Cirq type and the basic primitives.

#### Data/Cirq/Basic.hs

`Data.Cirq.Basic`

Reexports some of the functions from `Data.Cirq.Full`, but not the "low-level" ones.

#### Data/Cirq/Utils.hs

`Data.Cirq.Utils`

Defines some utility `Cirq`s.
Maybe a bad name?

#### Data/Cirq/Tools.hs

`Data.Cirq.Tools`

Defines some tools for manipulating `Cirq`s

#### Data/Nat.hs

Reexports instances and functions that don't clash with Prelude from Data.Nat.Internal

#### Data/Nat/Internal.hs

`Data.Nat.Internal`

Defines natural numbers inductively and operations on them.
Also defines instances of various classes for the natural number type.
Not intended for use. Use `Data.Nat` instead. (doesn't clash with Prelude)

Import qualified.

#### Data/Nat/Kind.hs

`Data.Nat.Kind`

Defines the type families needed to make type-level natural numbers work.

Requires TypeOperators and DataKinds.
*TODO: Test if it works without UndecidableInstances*

#### Data/Vector.hs

`Data.Vector`

Defines the `Vector` type, which represents n-element linked lists.
WIP, many functions not even thought up yet.

Requires DataKinds.
Uses GADTs and KindSignatures

*TODO: Consider if NoImplicitPrelude is necessary.*

#### Extra/Tuple.hs

`Extra.Tuple`

Defines some utility functions for tuples.
I think I heard someone say once on a forum (StackOverflow?) that a function like `sort` is a bad idea, and I should have a type for sorted tuples instead. Regardless, it's in there.

Some functions require RankNTypes and AllowAmbiguousTypes.
Uses KindSignatures and ConstraintKinds.

#### Extra/Bifunctor.hs

`Extra.Bifunctor`

Defines some of the functions from `Extra.Tuple` more generally, so they work on any bifunctor.

Most functions require RankNTypes and AllowAmbiguousTypes.
Uses KindSignatures and ConstraintKinds.

#### Extra/CirqUnpaced.hs

`Extra.CirqUnpacked`

Defines functions that run `Cirq`s from `Data.Cirq.Utils` on lists using `cqRun` from `Data.Cirq.Full`.

#### Extra/Function.hs

`Extra.Function`

Defines useful functions for manipulating functions.
Currently only defines higher-valence versions of `curry` and `uncurry`.

#### Extra/Maybe.hs

`Extra.Maybe`

Defines semi-useful functions concerning Maybe values.
They mostly just rid the values of their Maybe-ness.

#### Extra/Safe.hs

`Extra.Safe`

Defines safer versions of various Prelude and other standard library functions.

Import qualified.
Uses NoImplicitPrelude.

#### Deprecated.hs

`Deprecated`

Old functions that were rubbish, but whose code I have trouble letting go of.
Yes, it's ridiculous because of Git, but by golly who'nna predict when's gonna be useful 'gain? Ehm?

Doesn't export anything.
*TODO: should export something, maybe?*
Has a `module` header so the compiler stops complaining about the missing `main`.