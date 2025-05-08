module Test.Clash.ExternalLoop2 where

import Clash.Prelude hiding (fromList)
import Clash.CoSim.Yosys

external :: Int -> Int -> (Int, Int, Int)
external a b = (\(a, b, c) -> (unpack a, unpack b, unpack c)) $ $(externalComponentE (["a", "b"], ["c", "d", "e"]) "test/verilog/externalLoop2.v" defaultOptions) (pack b) (pack a)

topEntity:: Int
topEntity = out
  where
    (a, b, out) = external b a
