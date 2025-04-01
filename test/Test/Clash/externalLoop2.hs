module Test.Clash.ExternalLoop2 where

import Clash.Prelude hiding (fromList)
import Clash.CoSim.Yosys

import Interpreter hiding (compile)
import Util
import Memory hiding (BitVector)
import Intermediate
import Intermediate.CellKinds
import Data.Map (fromList)

external :: Int -> Int -> (Int, Int, Int)
external a b = (\(a, b, c) -> (unpack a, unpack b, unpack c)) $ $(externalComponentE (["a", "b"], ["c", "d", "e"]) "test/verilog/externalLoop2.v" defaultOptions) (pack b) (pack a)

external' :: Int -> Int -> (Int, Int, Int)
external' a b = (42, a, b)

topEntity1 :: Int
topEntity1 = out
  where
    (out, b, a) = external b a
