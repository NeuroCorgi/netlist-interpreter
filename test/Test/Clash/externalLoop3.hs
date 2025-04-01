module Test.Clash.ExternalLoop3 where

import Clash.Prelude
import Clash.CoSim.Yosys

t1 (a, _, _) = a
t2 (_, b, _) = b
t3 (_, _, c) = c

unzip3Signal :: Signal dom (a, b, c) -> (Signal dom a, Signal dom b, Signal dom c)
unzip3Signal s = (t1 <$> s, t2 <$> s, t3 <$> s)

type I = BitVector 64

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
    (d, e, f) = $(externalComponentE (["clk", "a", "b", "c"], ["d", "e", "f"]) "test/verilog/externalLoop3.v" defaultOptions) clk (fmap pack a) (fmap pack b) (fmap pack c)

topEntity :: KnownDomain dom => Clock dom -> Signal dom I -> Signal dom I
topEntity clk a = f
  where
    (d, e, f) = external clk a d e
