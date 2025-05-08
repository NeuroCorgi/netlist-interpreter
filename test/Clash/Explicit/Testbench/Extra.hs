{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Clash.Explicit.Testbench.Extra (AssertResult(..), outputVerifierT) where

import Control.Exception (catch, evaluate)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Clash.Explicit.Prelude

data AssertResult = Pass | Fail
  deriving (Eq, Show, Generic, NFDataX)

assertT
  :: (KnownDomain dom, Eq a, ShowX a)
  => Clock dom
  -> Signal dom a
  -- ^ Checked value
  -> Signal dom a
  -- ^ Expected value
  -> Signal dom b
  -- ^ Return value
  -> Signal dom (b, AssertResult)
assertT clk checked expected returned =
  (\c e cnt r ->
      if eqX c e
         then (r, Pass)
         else (r, Fail))
  <$> checked <*> expected <*> fromList [(0::Integer)..] <*> returned
  where
    eqX a b = unsafeDupablePerformIO (catch (evaluate (a == b))
                                            (\(_ :: XException) -> return False))

outputVerifierT
  :: forall l a dom
   . ( KnownNat l
     , KnownDomain dom
     , Eq a
     , ShowX a
     , 1 <= l
     )
  => Clock dom
  -- ^ Clock to which the test bench is synchronized (but not necessarily
  -- the circuit under test)
  -> Reset dom
  -- ^ Reset line of test bench
  -> Vec l a
  -- ^ Samples to compare with
  -> Signal dom a
  -- ^ Signal to verify
  -> Signal dom (Bool, AssertResult)
  -- ^ True if all samples are verified
outputVerifierT clk rst samples i0 =
    let en    = toEnable (pure True)
        (s,o) = unbundle (genT <$> register clk rst en 0 s)
        (e,f) = unbundle o
        f'    = register clk rst en False f
        -- Only assert while not finished
    in  mux f' ((, Pass) <$> f') $ assertT clk i0 e f'
  where
    genT :: Index l -> (Index l,(a,Bool))
    genT s = (s',(samples !! s,finished))
      where
        s' = satSucc SatBound s
        finished = s == maxBound
