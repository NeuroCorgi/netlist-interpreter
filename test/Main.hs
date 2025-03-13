import Test.Tasty
import Test.Tasty.HUnit

import Interp.Main

testFile :: FilePath -> TestTree
testFile designName = testCase designName $ do
  let designPath = "test/" ++ designName ++ ".v"
  let controlPath = "test/" ++ designName ++ ".control"
  modules <- readDesign designPath
  design <- liftEither $ compile modules
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
  , testFile "t1669"
  , testFile "t2220"
  , testFile "findIndex"
  , testFile "minimum"
  , testFile "counter"
  , testFile "clz"
  , testFile "loop2"
  ]

main :: IO ()
main = defaultMain tests
