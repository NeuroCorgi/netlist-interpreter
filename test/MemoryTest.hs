module MemoryTest (tests) where

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
            bv = bitView [2, 3, 4]
         in (mem ! bv) @?= [H, L, L],
      testCase "memory write 1" $
        let mem = memoryC [H, H, L, H]
            bv = bitView [1, 2, 3]
            mem' = memory $ mem // (bv, [L, L, L])
         in mem' @?= [H, L, L, L],
      testCase "memory write 2" $
        let mem = memoryC [H, H, H, H]
            bv = bitView [1, 2, 3]
         in ((mem // (bv, [L, L, L])) ! bv) @?= [L, L, L]
    ]

testUnary :: TestTree
testUnary =
  testGroup
    "unary operations"
    [ testCase "logical not of zero" $
        let mem = memoryC [L, H, L, L, L]
            abv = bitView [2, 3, 4]
            ybv = bitView [1, 0]
         in ((((!|) abv) ybv mem) ! ybv) @?= [H, L],
      testCase "logical nor of not zero" $
        let mem = memoryC [H, H, L, H, L]
            abv = bitView [2, 3, 4]
            ybv = bitView [0, 1]
         in ((((!|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "negation of zero" $
      let mem = memoryC [H, H, H, L, L]
          abv = bitView [3, 4]
          ybv = bitView [0, 1, 2]
      in ((((-|) abv) ybv mem) ! ybv) @?= [L, L, L]
    , testCase "negation of non zero 1" $
      let mem = memoryC [H, H, H, H, L]
          abv = bitView [3, 4]
          ybv = bitView [0, 1, 2]
      in ((((-|) abv) ybv mem) ! ybv) @?= [H, H, H]
    , testCase "negation of non zero 2" $
      let mem = memoryC [H, H, H, H, H]
          abv = bitView [3, 4]
          ybv = bitView [0, 1, 2]
      in ((((-|) abv) ybv mem) ! ybv) @?= [H, L, H]
    , testCase "bitwise not of zero" $
      let mem = memoryC [H, L, L]
          abv = bitView [1, 2]
          ybv = bitView [0, 1, 2]
      in ((((~|) abv) ybv mem) ! ybv) @?= [H, H, H]
    , testCase "bitwise not of all ones" $
      let mem = memoryC [H, H, L, L]
          abv = bitView [0, 1]
          ybv = bitView [2, 3]
      in ((((~|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "bitwise not 1" $
      let mem = memoryC [H, H, L, H]
          abv = bitView [2, 3]
          ybv = bitView [0, 1]
      in ((((~|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "bitwise not 2" $
      let mem = memoryC [H, H, H, H]
          abv = bitView [ 3 ]
          ybv = bitView [0, 1, 2]
      in ((((~|) abv) ybv mem) ! ybv) @?= [L, H, H]
    , testCase "positive" $
      let mem = memoryC [L, L, L, L, H, L, H]
          abv = bitView [ 4, 5, 6]
          ybv = bitView [ 0, 1, 2, 3]
      in ((((+|) abv) ybv mem) ! ybv) @?= [H, L, H, L]
    , testCase "reduce and all ones" $
    let mem = memoryC [L, L, H, H, H]
        abv = bitView [2, 3, 4]
        ybv = bitView [0, 1]
    in ((((&/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce and with zero" $
    let mem = memoryC [L, H, H, L, H]
        abv = bitView [2, 3, 4]
        ybv = bitView [0, 1]
    in ((((&/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "boolean reduce of zero" $
        let mem = memoryC [H, H, L, L, L]
            abv = bitView [2, 3, 4]
            ybv = bitView [0, 1]
        in ((((&/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "boolean reduce of non zero" $
      let mem = memoryC [L, L, H, H, H]
          abv = bitView [2, 3, 4]
          ybv = bitView [0, 1]
      in ((((&/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce or with all zeros" $
      let mem = memoryC [H, H, L, L, L]
          abv = bitView [2, 3, 4]
          ybv = bitView [0, 1]
      in ((((|/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce or with one" $
      let mem = memoryC [L, H, L, H, L]
          abv = bitView [2,3,4]
          ybv = bitView [0, 1]
      in ((((|/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with all zeros" $
      let mem = memoryC [L, H, L, L, L]
          abv = bitView [2, 3, 4]
          ybv = bitView [0, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with all ones" $
      let mem = memoryC [L, H, H, H, H]
          abv = bitView [2, 3, 4]
          ybv = bitView [0, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with mixed ones and zeros" $
      let mem = memoryC [L, H, L, H, L]
          abv = bitView [2, 3, 4]
          ybv = bitView [0, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce xor 1" $
      let mem = memoryC [L, H, L, H, H]
          abv = bitView [2, 3, 4]
          ybv = bitView [0, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce xor 2" $
      let mem = memoryC [L, H, L, L, H]
          abv = bitView [2, 3, 4]
          ybv = bitView [0, 1]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    ]

testBinary :: TestTree
testBinary =
  testCase "binary operations"
    (assertFailure "not implemented")

testMux :: TestTree
testMux =
  testGroup
    "multiplexers"
    [
      -- testCase "bmux" $
      -- assertFailure "not implemented"
      testCase "bwmux 1" $
      let mem = memoryC [L, L, H, H, H, L, X, X]
          bbv = bitView [2, 3]
          abv = bitView [1, 0]
          sbv = bitView [4, 5]
          ybv = bitView [6, 7]
      in ((bwmux abv bbv sbv ybv mem) ! ybv) @?= [L, H]
    , testCase "bwmux 2" $
      let mem = memoryC [L, L, H, H, L, Z, X, X]
          abv = bitView [0, 1]
          bbv = bitView [2, 3]
          sbv = bitView [4, 5]
          ybv = bitView [6, 7]
      in ((bwmux abv bbv sbv ybv mem) ! ybv) @?= [H, X]
    , testCase "mux select first" $
      let mem = memoryC [L, L, H, H, H, H, L]
          abv = bitView [0, 1]
          bbv = bitView [2, 3]
          sbv = bitView [ 4 ]
          ybv = bitView [5, 6]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [L, L]
    , testCase "mux select second" $
      let mem = memoryC [L, L, H, H, L, H, L]
          abv = bitView [0, 1]
          bbv = bitView [2, 3]
          sbv = bitView [ 4 ]
          ybv = bitView [5, 6]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [H, H]
    , testCase "mux select unknown" $
      let mem = memoryC [L, L, H, H, X, H, L]
          abv = bitView [0, 1]
          bbv = bitView [2, 3]
          sbv = bitView [ 4 ]
          ybv = bitView [5, 6]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [X, X]
    , testCase "demux 1" $
      let mem = memoryC [H, L, H, Z, Z, Z, Z]
          abv = bitView [ 0 ]
          sbv = bitView [1, 2]
          ybv = bitView [3, 4, 5, 6]
      in ((demux abv sbv ybv mem) ! ybv) @?= [L, L, H, L]
    , testCase "demux 2" $
      let mem = memoryC [H, H, L, Z, Z, Z, Z]
          abv = bitView [ 0 ]
          sbv = bitView [1, 2]
          ybv = bitView [3, 4, 5, 6]
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
