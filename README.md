## My personal Haskell repository

### **Upfront warning**

**All modules here are designed to be imported as follows:**

    import qualified Module as Mod
    import Module (function)

We don't avoid name clashing.

#

### Content of this README

* Purpose
* Explaining files
  * notesonhaskell.txt
  * Numberguessing.hs
  * Data
    * Cirq.hs
    * Cirq
      * Full.hs
        * Basic.hs
        * Utils.hs
        * Tools.hs
        * Deprecated.hs
    * Nat.hs
    * Nat
      * Kind.hs
  * Extra
    * Tuple.hs
    * Bifunctor.hs
    * CirqUnpacked.hs
    * Function.hs
    * Maybe.hs
    * Safe.hs

### Purpose

I use this to write any code that's not for a project that already has a folder.
Most projects, actually, are stored inside this folder, since it helps me to avoid duplicate module names.

### Explaining files

#### notesonhaskell.txt

*Inconsequential* ramblings, notes, and other stuff.

#### Numberguessing.hs

Generic number guessing game. In progress, I want to make some modules to allow me to get an arbitrary number of command-line arguments.
Will be invoked with two arguments denoting the lower and upper bounds of the number to be guessed.

#### Data/Cirq.hs

`Data.Cirq`

Exports all functions defined in the modules in the `Cirq` *folder* as a neat, complete package.

Don't import alongside the other Cirq modules.
No reason for that, it's just bad style and bad practice.

#### Data/Cirq/Full.hs

`Data.Cirq.Full`

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

#### Data/Cirq/Deprecated.hs

`Data.Cirq.Deprecated`

Old functions that were rubbish, but whose code I have trouble letting go of.
Thinking of either deleting this or moving it to its own folder, so other folder's "deprecated" can land there too.

Doesn't export anything.
Has a `module` header so the compiler stops complaining about the missing `main`.

#### Data/Nat/Internal.hs

`Data.Nat.Internal`

Defines natural numbers inductively and operations on them.
Also defines instances of various classes for the natural number type.
Not intended for use. Use `Data.Nat` instead.

Import qualified.

TODO: Consider splitting off a Nat module that doesn't clash with `Prelude`

#### Data/Nat/Kind.hs

`Data.Nat.Kind`

Defines the type families needed to make type-level natural numbers work.

Requires UndecidableInstances, TypeOperators and DataKinds.

#### Data/Vector.hs

`Data.Vector`

Defines the `Vector` type, which represents n-element linked lists.
WIP, many functions not even thought up yet.

Requires DataKinds.
Uses GADTs and KindSignatures

TODO: Consider if NoImplicitPrelude is necessary.

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