module Test.Clash.BlockRam where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.CoSim.Yosys

topEntity
  :: KnownDomain dom
  => Clock dom
  -> Enable dom
  -> Signal dom (Unsigned 2)
  -> Signal dom (Maybe (Unsigned 2, Unsigned 4))
  -> Signal dom (Unsigned 4, Unsigned 4)
topEntity clk en rd wrM = (,) <$> fmap unpack r0 <*> fmap unpack r1
  where
    (r0, r1) = ($(externalComponentE (["clk", "en", "rd", "wrM"], ["result_0", "result_1"]) "test/verilog/blockRam.v" defaultOptions) clk en (fmap pack rd) (fmap pack wrM))

samples =
  -- rd  wrM          out
  
     -- Read initial contents
     (0, Nothing     , 15)
  :> (1, Nothing     ,  4)
  :> (2, Nothing     ,  5)
     -- Write and read back
  :> (3, Just (0,  8),  6)
  :> (0, Just (1,  9),  7)
  :> (1, Just (2, 10),  8)
  :> (2, Just (3, 11),  9)
  :> (3, Nothing     , 10)
  :> (3, Nothing     , 11)
  :> Nil

testBench :: Signal System Bool
testBench = done
  where
    (rd, wrM, expect) = unzip3 samples
    rdInput = stimuliGenerator clk rst rd
    wrMInput = stimuliGenerator clk rst wrM
    expectedOutput =
      outputVerifier' clk rst $ zip expect expect
    done = expectedOutput $ ignoreFor clk rst en d1 (15, 15) $
             topEntity clk en rdInput wrMInput
    clk = tbSystemClockGen (not <$> done)
    rst = systemResetGen
    en = enableGen
