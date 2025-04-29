import Prelude

import System.IO
import System.Exit
import System.Posix.Types
import System.Posix.Process
import System.Posix.IO

import Control.Monad (when)

import Foreign.C.Types

import Clash.Prelude (Signal, System, sample)

import Test.Tasty
import Test.Tasty.HUnit

import Test.Clash.Fibonacci
import Test.Clash.BlockRam
-- import Test.Clash.T1669
import Test.Clash.LotsOfState
-- import Test.Clash.FindIndex
-- import test.clash.externalloop2

runTestbench :: Signal System Bool -> IO ()
runTestbench testbench = do
  (readFd, writeFd) <- createPipe
  pId <- forkProcess $ do
    dupTo writeFd stdError
    let samplesLength = length $ takeWhile not $ sample testbench

    closeFd writeFd
    exitWith (ExitFailure samplesLength)

  Just (Exited (ExitFailure nSamples)) <- getProcessStatus True False pId
  readH <- fdToHandle readFd
  wasError <- not <$> hReady readH

  hClose readH
  assertBool "testbench produced mismatch" wasError

tests :: TestTree
tests = testGroup ""
  [ testGroup "synchronous"
    [ testCase "fibonacci" $ runTestbench Test.Clash.Fibonacci.testBench
    , testCase "blockRam" $ runTestbench Test.Clash.BlockRam.testBench
    , testCase "lot of state" $ runTestbench Test.Clash.LotsOfState.testBench
    ]
  , testGroup "asynchronous"
    []
  --   testFile "lotsOfState"
  --   testFile "fibonacci"
  -- , testFile "genericBitPack"
  -- , testFile "registerAE"
  -- , testFile "t1669"
  -- , testFile "t2220"
  -- , testFile "findIndex"
  -- , testFile "minimum"
  -- , testFile "counter"
  -- , testFile "clz"
  -- , testFile "loop2"
  ]

main :: IO ()
main = defaultMain tests
