{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Clash.T1669 where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.Explicit.Testbench.Extra

import GHC.Generics()

import Clash.CoSim.Yosys

data AB = A | B deriving (Eq, Generic, BitPack)

topEntity :: AB -> Int -> Int
topEntity ab x = unpack $ $(externalComponentE ([("ab"), ("x")], [("result")]) "test/verilog/t1669.v" defaultOptions) (pack ab) (pack x)

testBench :: Signal System (Bool, AssertResult)
testBench = done
  where
    testInput      = stimuliGenerator clk rst ((A,1669):>(B,42):>Nil)
    expectedOutput = outputVerifierT clk rst (-99021 :> -93726 :> Nil)
    done           = expectedOutput (fmap (uncurry topEntity) testInput)
    clk            = tbSystemClockGen ((not . fst) <$> done)
    rst            = systemResetGen
