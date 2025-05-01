module Test.Clash.Fibonacci where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.CoSim.Yosys

topEntity :: KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> Signal dom (Unsigned 64)
topEntity clk rst en = fmap unpack $ $(externalComponentE (["eta", "eta1", "eta2"], ["result"]) "test/verilog/fibonacci.v" defaultOptions) clk rst en

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst $(listToVecTH [1 :: Unsigned 64,1,2,3,5])
    done           = expectedOutput (topEntity clk rst enableGen)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
