module Test.Unit.Memory (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Bit
import Memory

memoryC :: [Bit] -> Memory
memoryC mem = Memory {memory=mem, updated=[]}

testAccess :: TestTree
testAccess =
  testGroup
    "memory access"
    [ testCase "bitvector access" $
        let mem = memoryC [H, H, H, L, L]
            bv = toBitVector [2 :: Int, 3, 4]
         in (mem ! bv) @?= [H, L, L],
      testCase "memory write 1" $
        let mem = memoryC [H, H, L, H]
            bv = toBitVector [1 :: Int, 2, 3]
            mem' = memory $ mem // (bv, [L, L, L])
         in mem' @?= [H, L, L, L],
      testCase "memory write 2" $
        let mem = memoryC [H, H, H, H]
            bv = toBitVector [1 :: Int, 2, 3]
         in ((mem // (bv, [L, L, L])) ! bv) @?= [L, L, L]
    ]

testUnary :: TestTree
testUnary =
  testGroup
    "unary operations"
    [ testCase "logical not of zero" $
        let mem = memoryC [L, H, L, L, L]
            abv = toBitVector [2 :: Int, 3, 4]
            ybv = toBitVector [0 :: Int, 1]
         in ((((!|) abv) ybv mem) ! ybv) @?= [H, L],
      testCase "logical nor of not zero" $
        let mem = memoryC [H, H, L, H, L]
            abv = toBitVector [ 2 :: Int, 3, 4]
            ybv = toBitVector [ 0 :: Int, 1]
         in ((((!|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "negation of zero" $
      let mem = memoryC [H, H, H, L, L]
          abv = toBitVector [ 3 :: Int, 4]
          ybv = toBitVector [ 0 :: Int, 1, 2]
      in ((((-|) abv) ybv mem) ! ybv) @?= [L, L, L]
    , testCase "negation of non zero 1" $
      let mem = memoryC [H, H, H, H, L]
          abv = toBitVector [ 3 :: Int, 4]
          ybv = toBitVector [ 0 :: Int, 1, 2]
      in ((((-|) abv) ybv mem) ! ybv) @?= [H, H, H]
    , testCase "negation of non zero 2" $
      let mem = memoryC [H, H, H, H, H]
          abv = toBitVector [ 3 :: Int, 4]
          ybv = toBitVector [ 0 :: Int, 1, 2]
      in ((((-|) abv) ybv mem) ! ybv) @?= [H, L, H]
    , testCase "bitwise not of zero" $
      let mem = memoryC [H, L, L]
          abv = toBitVector [ 1 :: Int, 2]
          ybv = toBitVector [ 0 :: Int, 1, 2]
      in ((((~|) abv) ybv mem) ! ybv) @?= [H, H, H]
    , testCase "bitwise not of all ones" $
      let mem = memoryC [H, H, L, L]
          abv = toBitVector [ 0 :: Int, 1]
          ybv = toBitVector [ 2 :: Int, 3]
      in ((((~|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "bitwise not 1" $
      let mem = memoryC [H, H, L, H]
          abv = toBitVector [ 2 :: Int, 3]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((~|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "bitwise not 2" $
      let mem = memoryC [H, H, H, H]
          abv = toBitVector [ 3 :: Int ]
          ybv = toBitVector [ 0 :: Int, 1, 2]
      in ((((~|) abv) ybv mem) ! ybv) @?= [L, H, H]
    , testCase "positive" $
      let mem = memoryC [L, L, L, L, H, L, H]
          abv = toBitVector [ 4 :: Int, 5, 6]
          ybv = toBitVector [ 0 :: Int, 1, 2, 3]
      in ((((+|) abv) ybv mem) ! ybv) @?= [H, L, H, L]
    , testCase "reduce and all ones" $
    let mem = memoryC [L, L, H, H, H]
        abv = toBitVector [ 2 :: Int, 3, 4]
        ybv = toBitVector [ 0 :: Int, 1]
    in ((((&/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce and with zero" $
    let mem = memoryC [L, H, H, L, H]
        abv = toBitVector [ 2 :: Int, 3, 4]
        ybv = toBitVector [ 0 :: Int, 1]
    in ((((&/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "boolean reduce of zero" $
        let mem = memoryC [H, H, L, L, L]
            abv = toBitVector [ 2 :: Int, 3, 4]
            ybv = toBitVector [ 0 :: Int, 1]
        in ((((&/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "boolean reduce of non zero" $
      let mem = memoryC [L, L, H, H, H]
          abv = toBitVector [ 2 :: Int, 3, 4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((&/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce or with all zeros" $
      let mem = memoryC [H, H, L, L, L]
          abv = toBitVector [ 2 :: Int, 3, 4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((|/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce or with one" $
      let mem = memoryC [L, H, L, H, L]
          abv = toBitVector [ 2 :: Int,3,4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((|/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with all zeros" $
      let mem = memoryC [L, H, L, L, L]
          abv = toBitVector [ 2 :: Int, 3, 4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with all ones" $
      let mem = memoryC [L, H, H, H, H]
          abv = toBitVector [ 2 :: Int, 3, 4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with mixed ones and zeros" $
      let mem = memoryC [L, H, L, H, L]
          abv = toBitVector [ 2 :: Int, 3, 4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce xor 1" $
      let mem = memoryC [L, H, L, H, H]
          abv = toBitVector [ 2 :: Int, 3, 4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce xor 2" $
      let mem = memoryC [L, H, L, L, H]
          abv = toBitVector [ 2 :: Int, 3, 4]
          ybv = toBitVector [ 0 :: Int, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    ]

testBinary :: TestTree
testBinary =
  testGroup "binary operations"
  [ testCase "addition 1: pos + pos" $
    let mem = memoryC [H, L, H, L, H, L, X, X, X, X, X]
        abv = toBitVector [ 0 :: Int, 1, 2, 3 ]
        bbv = toBitVector [ 4 :: Int, 5 ]
        ybv = toBitVector [ 6 :: Int, 7, 8, 9, 10 ]
     in (((abv |+| bbv) ybv mem) ! ybv) @?= [L, H, H, L, L]
  , testCase "addition 2: pos + neg" $
    let mem = memoryC [H, L, H, L, H, X, X, X]
        abv = toBitVector ([ 0 :: Int, 1 ], True)
        bbv = toBitVector ([ 2 :: Int, 3, 4 ], True)
        ybv = toBitVector [ 5 :: Int, 6, 7 ]
     in (((abv |+| bbv) ybv mem) ! ybv) @?= [L, H, H]
  , testCase "addition 3: neg + pos" $
    let mem = memoryC [H, H, H, L, L, X, X, X, X, X]
        abv = toBitVector ([ 0 :: Int, 1 ], True)
        bbv = toBitVector ([ 2 :: Int, 3, 4 ], True)
        ybv = toBitVector [ 5 :: Int, 6, 7, 8, 9 ]
     in (((abv |+| bbv) ybv mem) ! ybv) @?= [L, L, L, L, L]
  , testCase "addition 4" $
    let mem = memoryC [H, H, H, L, H, X, X, X, X, X]
        abv = toBitVector [ 0 :: Int, 1 ]
        bbv = toBitVector ([ 2 :: Int, 3, 4 ], True)
        ybv = toBitVector [ 5 :: Int, 6, 7, 8, 9 ]
     in (((abv |+| bbv) ybv mem) ! ybv) @?= [L, L, L, H, L]
  , testCase "addition 5: neg + neg" $
    let mem = memoryC [H, H, H, L, H, X, X, X, X, X]
        abv = toBitVector ([ 0 :: Int, 1 ], True)
        bbv = toBitVector ([ 2 :: Int, 3, 4 ], True)
        ybv = toBitVector [ 5 :: Int, 6, 7, 8, 9 ]
     in (((abv |+| bbv) ybv mem) ! ybv) @?= [L, L, H, H, H]
  ]

testMux :: TestTree
testMux =
  testGroup
    "multiplexers"
    [
      -- testCase "bmux" $
      -- assertFailure "not implemented"
      testCase "bwmux 1" $
      let mem = memoryC [L, L, H, H, H, L, X, X]
          bbv = toBitVector [ 2 :: Int, 3]
          abv = toBitVector [ 1 :: Int, 0]
          sbv = toBitVector [ 4 :: Int, 5]
          ybv = toBitVector [ 6 :: Int, 7]
      in ((bwmux abv bbv sbv ybv mem) ! ybv) @?= [L, H]
    , testCase "bwmux 2" $
      let mem = memoryC [L, L, H, H, L, Z, X, X]
          abv = toBitVector [ 0 :: Int, 1]
          bbv = toBitVector [ 2 :: Int, 3]
          sbv = toBitVector [ 4 :: Int, 5]
          ybv = toBitVector [ 6 :: Int, 7]
      in ((bwmux abv bbv sbv ybv mem) ! ybv) @?= [H, X]
    , testCase "mux select first" $
      let mem = memoryC [L, L, H, H, H, H, L]
          abv = toBitVector [ 0 :: Int, 1]
          bbv = toBitVector [ 2 :: Int, 3]
          sbv = toBitVector [ 4 :: Int ]
          ybv = toBitVector [ 5 :: Int, 6]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [H, H]
    , testCase "mux select second" $
      let mem = memoryC [L, L, H, H, L, H, L]
          abv = toBitVector [ 0 :: Int, 1]
          bbv = toBitVector [ 2 :: Int, 3]
          sbv = toBitVector [ 4 :: Int ]
          ybv = toBitVector [ 5 :: Int, 6]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [L, L]
    , testCase "mux select unknown" $
      let mem = memoryC [L, L, H, H, X, H, L]
          abv = toBitVector [ 0 :: Int, 1]
          bbv = toBitVector [ 2 :: Int, 3]
          sbv = toBitVector [ 4 :: Int ]
          ybv = toBitVector [ 5 :: Int, 6]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [Z, Z]
    , testCase "demux 1" $
      let mem = memoryC [H, L, H, Z, Z, Z, Z]
          abv = toBitVector [ 0 :: Int ]
          sbv = toBitVector [ 1 :: Int, 2]
          ybv = toBitVector [ 3 :: Int, 4, 5, 6]
      in ((demux abv sbv ybv mem) ! ybv) @?= [L, L, H, L]
    , testCase "demux 2" $
      let mem = memoryC [H, H, L, Z, Z, Z, Z]
          abv = toBitVector [ 0 :: Int ]
          sbv = toBitVector [ 1 :: Int, 2]
          ybv = toBitVector [ 3 :: Int, 4, 5, 6]
      in ((demux abv sbv ybv mem) ! ybv) @?= [L, H, L, L]
    ]

tests :: TestTree
tests =
  testGroup
    "memory operations"
    [ testAccess
    , testUnary
    , testBinary
    , testMux
    ]
