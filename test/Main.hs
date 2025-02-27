import Test.Tasty
import Test.Tasty.HUnit

import Interp.Main

testFile :: FilePath -> TestTree
testFile designName = testCase designName $ do
  let designPath = "test/" ++ designName ++ ".v"
  let controlPath = "test/" ++ designName ++ ".control"
  design <- readDesign designPath
  control <- readCommands controlPath
  _ <- exec design control
  return ()

tests :: TestTree
tests = testGroup ""
  [
    -- testFile "lotsOfState"
    testFile "fibonacci"
  , testFile "genericBitPack"
  , testFile "registerAE"
  ]

main :: IO ()
main = defaultMain tests
