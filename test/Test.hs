import Test.Tasty

import qualified Test.Unit.Memory as MemoryTest

tests :: TestTree
tests = testGroup ""
  [ MemoryTest.tests
  ]

main :: IO ()
main = defaultMain tests
