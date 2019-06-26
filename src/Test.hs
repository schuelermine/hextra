-- Intended to test if you can export things you don't import. No you can't.
-- ! Non-functional

module Test(Cirq(Cirq)) where

import Cirq.Basic