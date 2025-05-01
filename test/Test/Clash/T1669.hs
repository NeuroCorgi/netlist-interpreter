{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Clash.T1669 where

import Clash.Prelude
import Clash.Explicit.Testbench

import GHC.Generics

import Clash.CoSim.Yosys

import Debug.Trace

data AB = A | B deriving (Eq, Generic, BitPack)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}
-- topEntity :: AB -> Int -> Int
-- topEntity ab x = unpack $ $(externalComponentE ([("ab"), ("x")], [("result")]) "test/verilog/t1669.v" defaultOptions) (pack ab) (pack x)

testBench :: Signal System Bool
testBench = trace "not emplemented" $ pure True
-- testBench = done
--   where
--     testInput      = stimuliGenerator clk rst ((A,1669):>(B,42):>Nil)
--     expectedOutput = outputVerifier' clk rst (-99021 :> -93726 :> Nil)
--     done           = expectedOutput (fmap (uncurry topEntity) testInput)
--     clk            = tbSystemClockGen (not <$> done)
--     rst            = systemResetGen
