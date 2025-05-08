module Test.Clash.ExternalLoop where

import Clash.Prelude hiding (fromList)
import Clash.CoSim.Yosys

import Interpreter hiding (compile)
import Util
import Memory hiding (BitVector)
import Intermediate
import Intermediate.CellKinds
import Data.Map (fromList)

-- external to haskell, external verilog file
external :: Int -> Int -> (Int, Int)
external a b = (unpack c, unpack d)
  where
    (c, d) = $(externalComponentE (["a", "b"], ["c", "d"]) "test/verilog/externalLoop.v" defaultOptions{parameters=[("WIDTH", "64")]})
      (pack a) (pack b)

-- internal in here, just some operations
internal :: Int -> (Int, Int)
internal n = (-n * n, n + 5)

topEntity :: Int
topEntity = out
  where
    (out, ci) = external co1 co2
    (co1, co2) = internal ci

