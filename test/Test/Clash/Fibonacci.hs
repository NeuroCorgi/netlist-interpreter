{-# OPTIONS_GHC -ddump-splices #-}

module Test.Clash.Fibonacci where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Explicit.Testbench.Extra

import Clash.CoSim.Yosys

external :: KnownDomain dom => Clock dom -> Reset dom -> Enable dom -> Signal dom (Unsigned 64)
external clk rst en = fmap unpack $ $(externalComponentE (["eta", "eta1", "eta2"], ["result"]) "test/verilog/fibonacci.v" defaultOptions) clk rst en

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Unsigned 64)
topEntity = external

testBench :: Signal System (Bool, AssertResult)
testBench = done
  where
    expectedOutput = outputVerifierT clk rst $(listToVecTH [1 :: Unsigned 64,1,2,3,5])
    done           = expectedOutput (topEntity clk rst enableGen)
    clk            = tbSystemClockGen ((not . fst) <$> done)
    rst            = systemResetGen
