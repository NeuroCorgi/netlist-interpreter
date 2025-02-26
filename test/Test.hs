import Test.Tasty

import qualified MemoryTest


tests :: TestTree
tests = testGroup ""
  [ MemoryTest.tests
  ]

main :: IO ()
main = defaultMain tests
