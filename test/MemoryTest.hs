module MemoryTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Bit
import Memory

testAccess :: TestTree
testAccess =
  testGroup
    "memory access"
    [ testCase "bitvector access" $
        let mem = Memory [H, H, H, L, L]
            bv = BitVector [4, 3, 2]
         in (mem ! bv) @?= [H, L, L],
      testCase "memory write 1" $
        let mem = Memory [H, H, L, H]
            bv = BitVector [3, 2, 1]
            Memory mem' = mem // (bv, [L, L, L])
         in mem' @?= [H, L, L, L],
      testCase "memory write 2" $
        let mem = Memory [H, H, H, H]
            bv = BitVector [3, 2, 1]
         in ((mem // (bv, [L, L, L])) ! bv) @?= [L, L, L]
    ]

testUnary :: TestTree
testUnary =
  testGroup
    "unary operations"
    [ testCase "logical not of zero" $
        let mem = Memory [L, H, L, L, L]
            abv = BitVector [4, 3, 2]
            ybv = BitVector [1, 0]
         in ((((!|) abv) ybv mem) ! ybv) @?= [H, L],
      testCase "logical nor of not zero" $
        let mem = Memory [H, H, L, H, L]
            abv = BitVector [4, 3, 2]
            ybv = BitVector [1, 0]
         in ((((!|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "negation of zero" $
      let mem = Memory [H, H, H, L, L]
          abv = BitVector [4, 3]
          ybv = BitVector [2, 1, 0]
      in ((((-|) abv) ybv mem) ! ybv) @?= [L, L, L]
    , testCase "negation of non zero 1" $
      let mem = Memory [H, H, H, H, L]
          abv = BitVector [4, 3]
          ybv = BitVector [2, 1, 0]
      in ((((-|) abv) ybv mem) ! ybv) @?= [H, H, H]
    , testCase "negation of non zero 2" $
      let mem = Memory [H, H, H, H, H]
          abv = BitVector [4, 3]
          ybv = BitVector [2, 1, 0]
      in ((((-|) abv) ybv mem) ! ybv) @?= [H, L, H]
    , testCase "bitwise not of zero" $
      let mem = Memory [H, L, L]
          abv = BitVector [2, 1]
          ybv = BitVector [2, 1, 0]
      in ((((~|) abv) ybv mem) ! ybv) @?= [H, H, H]
    , testCase "bitwise not of all ones" $
      let mem = Memory [H, H, L, L]
          abv = BitVector [1, 0]
          ybv = BitVector [3, 2]
      in ((((~|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "bitwise not 1" $
      let mem = Memory [H, H, L, H]
          abv = BitVector [3, 2]
          ybv = BitVector [1, 0]
      in ((((~|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "bitwise not 2" $
      let mem = Memory [H, H, H, H]
          abv = BitVector [ 3 ]
          ybv = BitVector [2, 1, 0]
      in ((((~|) abv) ybv mem) ! ybv) @?= [L, H, H]
    , testCase "positive" $
      let mem = Memory [L, L, L, L, H, L, H]
          abv = BitVector [ 6, 5, 4]
          ybv = BitVector [ 3, 2, 1, 0]
      in ((((+|) abv) ybv mem) ! ybv) @?= [H, L, H, L]
    , testCase "reduce and all ones" $
    let mem = Memory [L, L, H, H, H]
        abv = BitVector [4, 3, 2]
        ybv = BitVector [1, 0]
    in ((((&/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce and with zero" $
    let mem = Memory [L, H, H, L, H]
        abv = BitVector [4, 3, 2]
        ybv = BitVector [1, 0]
    in ((((&/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "boolean reduce of zero" $
        let mem = Memory [H, H, L, L, L]
            abv = BitVector [4, 3, 2]
            ybv = BitVector [1, 0]
        in ((((&/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "boolean reduce of non zero" $
      let mem = Memory [L, L, H, H, H]
          abv = BitVector [4, 3, 2]
          ybv = BitVector [1, 0]
      in ((((&/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce or with all zeros" $
      let mem = Memory [H, H, L, L, L]
          abv = BitVector [4, 3, 2]
          ybv = BitVector [1, 0]
      in ((((|/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce or with one" $
      let mem = Memory [L, H, L, H, L]
          abv = BitVector [4,3,2]
          ybv = BitVector [1, 0]
      in ((((|/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with all zeros" $
      let mem = Memory [L, H, L, L, L]
          abv = BitVector [4, 3, 2]
          ybv = BitVector [1, 0]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with all ones" $
      let mem = Memory [L, H, H, H, H]
          abv = BitVector [4, 3, 2]
          ybv = BitVector [1, 0]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [H, L]
    , testCase "reduce xnor with mixed ones and zeros" $
      let mem = Memory [L, H, L, H, L]
          abv = BitVector [4, 3, 2]
          ybv = BitVector [1, 0]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce xor 1" $
      let mem = Memory [L, H, L, H, H]
          abv = BitVector [4, 3, 2]
          ybv = BitVector [1, 0]
      in ((((!^/|) abv) ybv mem) ! ybv) @?= [L, L]
    , testCase "reduce xor 2" $
      let mem = Memory [L, H, L, L, H]
          abv = BitVector [4, 3, 2]
          ybv = BitVector [1, 0]
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
      let mem = Memory [L, L, H, H, H, L, X, X]
          abv = BitVector [1, 0]
          bbv = BitVector [3, 2]
          sbv = BitVector [5, 4]
          ybv = BitVector [7, 6]
      in ((bwmux abv bbv sbv ybv mem) ! ybv) @?= [L, H]
    , testCase "bwmux 2" $
      let mem = Memory [L, L, H, H, L, Z, X, X]
          abv = BitVector [1, 0]
          bbv = BitVector [3, 2]
          sbv = BitVector [5, 4]
          ybv = BitVector [7, 6]
      in ((bwmux abv bbv sbv ybv mem) ! ybv) @?= [H, X]
    , testCase "mux select first" $
      let mem = Memory [L, L, H, H, H, H, L]
          abv = BitVector [1, 0]
          bbv = BitVector [3, 2]
          sbv = BitVector [ 4 ]
          ybv = BitVector [6, 5]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [L, L]
    , testCase "mux select second" $
      let mem = Memory [L, L, H, H, L, H, L]
          abv = BitVector [1, 0]
          bbv = BitVector [3, 2]
          sbv = BitVector [ 4 ]
          ybv = BitVector [6, 5]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [H, H]
    , testCase "mux select unknown" $
      let mem = Memory [L, L, H, H, X, H, L]
          abv = BitVector [1, 0]
          bbv = BitVector [3, 2]
          sbv = BitVector [ 4 ]
          ybv = BitVector [6, 5]
      in ((mux abv bbv sbv ybv mem) ! ybv) @?= [X, X]
    , testCase "demux 1" $
      let mem = Memory [H, L, H, Z, Z, Z, Z]
          abv = BitVector [ 0 ]
          sbv = BitVector [2, 1]
          ybv = BitVector [6, 5, 4, 3]
      in ((demux abv sbv ybv mem) ! ybv) @?= [L, L, H, L]
    , testCase "demux 2" $
      let mem = Memory [H, H, L, Z, Z, Z, Z]
          abv = BitVector [ 0 ]
          sbv = BitVector [2, 1]
          ybv = BitVector [6, 5, 4, 3]
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
