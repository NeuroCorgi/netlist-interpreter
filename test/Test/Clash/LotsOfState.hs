-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Clash.LotsOfState where

import qualified Prelude

import Clash.Prelude
import Clash.Signal
import Clash.Explicit.Testbench
import Clash.Explicit.Testbench.Extra

import Clash.CoSim.Yosys

topEntity :: KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> Signal dom (Unsigned 8) -> Signal dom (Unsigned 8)
topEntity clk rst en sig = fmap unpack ($(externalComponentE (["eta", "eta1", "eta2", "eta_0"], ["result"]) "test/verilog/lotsOfState.v" defaultOptions) clk rst (fmap pack $ fromEnable en) (fmap pack sig))

testBench :: Signal System (Bool, AssertResult)
testBench = done
  where
    testInput = stimuliGenerator clk rst $(listToVecTH
        [0 :: (Unsigned 8), 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 1, 1])
    expectedOutput = outputVerifierT clk rst $(listToVecTH
        [0 :: (Unsigned 8), 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 0, 6, 0, 0, 7, 0, 0, 8, 0, 9, 0, 10,  0, 11,  0, 1, 0])
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen ((not . fst) <$> done)
    rst            = systemResetGen
