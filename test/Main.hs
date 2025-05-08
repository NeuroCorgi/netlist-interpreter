import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude (Signal, System, sample)
import Clash.Explicit.Testbench.Extra

import Test.Clash.Fibonacci
import Test.Clash.BlockRam
import Test.Clash.LotsOfState
import Test.Clash.T1669
import Test.Clash.ExternalLoop
import Test.Clash.ExternalLoop2
import Test.Clash.ExternalLoop3

runTestbench :: Signal System (Bool, AssertResult) -> IO ()
runTestbench testbench = assertBool "testbench produced mismatch" wasError
  where
    wasError = all ((== Pass) . snd) $ takeWhile (not . fst) $ sample testbench

tests :: TestTree
tests = testGroup ""
  [ testGroup "test benches"
    [ testCase "fibonacci" $ runTestbench Test.Clash.Fibonacci.testBench
    , testCase "blockRam" $ runTestbench Test.Clash.BlockRam.testBench
    , testCase "lot of states" $ runTestbench Test.Clash.LotsOfState.testBench
    , testCase "t1669" $ runTestbench Test.Clash.T1669.testBench
    , testCase "external loop 3" $ runTestbench Test.Clash.ExternalLoop3.testBench
    ]
  , testGroup "normal tests"
    [ testCase "external loop 1" $ Test.Clash.ExternalLoop.topEntity @?= (-85)
    , testCase "external loop 2" $ Test.Clash.ExternalLoop2.topEntity @?= 42
    ]
  ]

main :: IO ()
main = defaultMain tests
