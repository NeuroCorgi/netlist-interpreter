module Clash.CoSim.Yosys.Util
  ( toVectorBit
  , fromVectorBit
  , bits
  , zipLongest
  )
where

import Data.Function (on)

import Data.Vector (Vector)
import qualified Data.Vector as V (fromList)
import Clash.Sized.Internal.BitVector hiding (Bit)

import GHC.Num (integerToNatural)
import GHC.Natural (naturalToInteger)

import Memory (Bit(..))

import Internal.Util

toVectorBit :: BitVector n -> Vector Bit
toVectorBit (BV mask nat) = V.fromList $ map from $ ((zipLongest False) `on` (bits . naturalToInteger)) mask nat
  where
    from (True, True) = X
    from (True, False) = Z
    from (False, True) = H
    from (False, False) = L

bits :: Integer -> [Bool]
bits 0 = []
bits n
  | even n = False : bits nn
  | odd n = True : bits nn
  where nn = n `div` 2

zipLongest :: a -> [a] -> [a] -> [(a, a)]
zipLongest _ [] [] = []
zipLongest d [] (a : rest) = (d, a) : zipLongest d [] rest
zipLongest d (a : rest) [] = (a, d) : zipLongest d rest []
zipLongest d (a : as) (b : bs) = (a, b) : zipLongest d as bs

fromVectorBit :: Vector Bit -> BitVector n
fromVectorBit vect = let (mask, nat) = toNatPair vect in BV { unsafeMask = mask, unsafeToNatural = nat }
  where
    toNatPair vect = both integerToNatural $ foldr (\b (mask', value) -> (2 * mask' + mask b, 2 * value + val b)) (0 :: Integer, 0 :: Integer) vect
      where
        val H = 1
        val L = 0
        val _   = 0

        mask Z = 1
        mask X = 1
        mask _ = 0
