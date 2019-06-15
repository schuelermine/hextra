-- ! Redundant, by design
-- * Use with ghc -fno-code to test if all modules type-check
-- Command (Lnx): ghc -fno-code ./TestAll.hs
-- Command (Win): ghc -fno-code .\Testall.hs

import Data.Cirq
import Data.Cirq.Base
import Data.Cirq.Basic
import Data.Cirq.Tools
import Data.Cirq.Utils
import Data.Hextra.Nat
import Data.Hextra.Nat.Internal
import Data.Hextra.Nat.Kind
import Data.Hextra.Nat.Finite
import Data.Tree
import Data.Tree.Generalized
import Data.Sqc
import Data.Vector
import Extra
import Extra.Applicative
import Extra.Bifunctor
import Extra.CirqUnpacked
import Extra.Function
import Extra.Functor
import Extra.Group
import Extra.Integral
import Extra.List
import Extra.Maybe
import Extra.Monad
import Extra.Nat
import Extra.Num
import Extra.Safe
import Extra.Tuple
import Deprecated
import Useless

main :: IO ()
main = putStrLn ("Apparently, this program compiled successfully!")