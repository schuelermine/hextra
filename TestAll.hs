-- ! Redundant, by design
-- * Use with ghc -fno-code to test if all module type-check

import Cirq.Basic
import Cirq.Full
import Cirq.Tools
import Cirq.Utils
import qualified Data.Nat
import Data.Nat.Kind
import Data.Vector
import Extra.Bifunctor
import Extra.CirqUnpacked
import Extra.Function
import Extra.Maybe
import Extra.Tuple (dupe)

main :: IO ()
main = putStrLn ("Apparently, this program compiled successfully!")