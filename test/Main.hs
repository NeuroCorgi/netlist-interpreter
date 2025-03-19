import Test.Tasty
import Test.Tasty.HUnit

import Test.Clash.Fibonacci

tests :: TestTree
tests = testGroup ""
  [
    -- testFile "lotsOfState"
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
