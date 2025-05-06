-- {-# OPTIONS_GHC -ddump-splices #-}

module Test.Clash.ExternalLoop3 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import Clash.CoSim.Yosys

type I = BitVector 2

-- external
--   :: KnownDomain dom
--   => Clock dom
--   -> Signal dom I
--   -> Signal dom I
--   -> Signal dom I
--   -> Signal dom (I, I, I)
-- external clk a b c = fmap (\(a, b, c) -> (unpack a, unpack b, unpack c)) $ $(externalComponentE (["clk", "a", "b", "c"], ["d", "e", "f"]) "test/verilog/externalLoop3.v" defaultOptions) clk (fmap pack a) (fmap pack b) (fmap pack c)

external :: KnownDomain dom
  => Clock dom
  -> Signal dom I
  -> Signal dom I
  -> Signal dom I
  -> (Signal dom I, Signal dom I, Signal dom I)
external clk a b c = (d, e, f)
  where
    (d, e, f) = $(externalComponentE (["clk", "a", "b", "c"], ["d", "e", "f"]) "test/verilog/externalLoop3.v" defaultOptions{parameters=[("width", "2")]})
      clk (fmap pack a) (fmap pack b) (fmap pack c)

external' :: KnownDomain dom
  => Clock dom
  -> Signal dom I
  -> Signal dom I
  -> Signal dom I
  -> (Signal dom I, Signal dom I, Signal dom I)
external' clk a b c = (a, b, c)

topEntity :: KnownDomain dom => Clock dom -> Signal dom I -> Signal dom (I, I, I)
topEntity clk a = (,,) <$> d <*> e <*> f
  where
    (d, e, f) = external clk a d e

testBench :: Signal System Bool
testBench = done
  where
    samples = 0 :> 1 :> 2 :> Nil

    expectedOutput = outputVerifier' clk rst $ zip3 samples samples samples
    done = expectedOutput $ topEntity clk (stimuliGenerator clk rst samples)
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen

