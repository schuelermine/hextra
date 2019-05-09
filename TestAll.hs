-- ! Redundant, by design
-- * Use with ghc -fno-code to test if all modules type-check
-- Command (Lnx): ghc -fno-code ./TestAll.hs
-- Command (Win): ghc -fno-code .\Testall.hs

import Data.Cirq.Base
import Data.Cirq.Basic
import Data.Cirq.Deprecated
import Data.Cirq.Tools
import Data.Cirq.Utils
import Data.Nat
import Data.Nat.Compatible
import Data.Nat.Kind
import Data.Sqc
import Data.Vector
import Extra
import Extra.Bifunctor
import Extra.CirqUnpacked
import Extra.Function
import Extra.Integral
import Extra.Maybe
import Extra.Nat
import Extra.Num
import Extra.Safe
import Extra.Tuple

main :: IO ()
main = putStrLn ("Apparently, this program compiled successfully!")