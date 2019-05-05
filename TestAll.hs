-- ! Redundant, by design
-- * Use with ghc -fno-code to test if all module type-check

import Data.Cirq.Basic
import Data.Cirq.Full
import Data.Cirq.Tools
import Data.Cirq.Utils
import qualified Data.Nat.Internal
import Data.Nat.Instances
import Data.Nat.Kind
import Data.Vector
import Extra.Bifunctor
import Extra.CirqUnpacked
import Extra.Function
import Extra.Maybe
import Extra.Tuple (dupe)

main :: IO ()
main = putStrLn ("Apparently, this program compiled successfully!")