{-# LANGUAGE TemplateHaskell #-}

module Clash.CoSim.Yosys.Util
  ( toVectorBit
  , fromVectorBit
  , bits
  , zipLongest
  , unzipFN
  , unzipN
  )
where

import Language.Haskell.TH

import Data.Function (on)

import Data.Vector (Vector)
import qualified Data.Vector as V (fromList)
import Clash.Sized.Internal.BitVector hiding (Bit)

import GHC.Num (integerToNatural)
import GHC.Natural (naturalToInteger)

import Memory (Bit(..))

import Internal.Util

accs :: Int -> Int -> Q Exp
accs n m = do
  a <- newName "a"
  [| \ $(tupP [ if i == m then varP a else wildP | i <- [0..n - 1] ]) -> $(varE a) |]

unzipN :: Int -> Q Exp
unzip 1 = [| id |]
unzipN n = do
  s <- newName "s"
  [| \ $(varP s) -> $(tupE $ map (\i -> [| $(accs n i) $(varE s) |]) [0..n - 1] ) |]

unzipFN :: Int -> Q Exp
unzipFN 1 = [| id |]
unzipFN n = do
  s <- newName "s"
  [| \ $(varP s) -> $(tupE $ map (\i -> [| $(accs n i) <$> $(varE s) |]) [0..n - 1] ) |]

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
