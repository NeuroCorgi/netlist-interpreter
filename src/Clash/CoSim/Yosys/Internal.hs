{-# LANGUAGE TupleSections #-}

module Clash.CoSim.Yosys.Internal
  ( Role(..)
  , markRolesForInputs
  , reorder
  -- * Dependent group detection
  , CombDependent(..)
  , dependencies
  , markDependentOutputs
  , uniteDependentGroups
  -- * Doman detection
  , Domain
  , markDomains
  )
where

import Control.Arrow (first)

import qualified Data.List as List
import qualified Data.Map as Map (lookup, elems)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)

import qualified Data.Vector as V

import qualified Data.IntMap as IM
import qualified Data.Set as S

import Memory
import Intermediate
import Data.List (uncons)

data Role
  = Clock
  | Reset
  | Enable
  | Other
  deriving Eq

markRolesForInputs :: Module -> [(Port, Role)]
-- Multy domain design with several clock signals?
markRolesForInputs Module{modInputs=inputs, modCells=cells} = map mark inputs
  where
    -- Naive guesses for names of ports
    clock, reset, enable :: [String -> Bool]
    clock = map List.isPrefixOf ["clk", "clock"]
    reset = map List.isPrefixOf ["rst", "arst", "reset"]
    enable = map List.isPrefixOf ["en", "enable"]

    mark p@Port{pName=name, pBits=bits}
    -- First naive guess by the port name
      | like clock name = (p, Clock)
      | like reset name = (p, Reset)
      | like enable name = (p, Enable)
    -- Second guess by loking at internal cell inputs
      | cs == cellClockInputs = (p, Clock)
      | cs == cellResetInputs = (p, Reset)
      | cs == cellEnableInputs = (p, Enable)
      | otherwise = (p, Other)
      where cs = wires bits

    like f name = or $ f <*> return name

    cellInputs signal = List.nub . concatMap wires . V.mapMaybe (\Cell{cInputs=ins} -> Map.lookup signal ins)
    cellClockInputs = cellInputs "CLK" cells
    cellResetInputs = cellInputs "ARST" cells
    cellEnableInputs = cellInputs "EN" cells

reorder :: [(a, Role)] -> [(a, Role)]
reorder lst = clock ++ reset ++ enable ++ rest''
  where
    (clock, rest) = List.partition ((== Clock) . snd) lst
    (reset, rest') = List.partition ((== Reset) . snd) rest
    (enable, rest'') = List.partition ((== Enable) . snd) rest'

data CombDependent
  = NotDependent
  -- ^ Outputs does not depend on any input port or only depends on ports that are known to be independent
  | Combinatorial [String]
  -- ^ Outputs depends on the list of input ports that can be combinatorial to the outputs and possibly on some independent port inputs
  deriving Eq

combineGroup :: [CombDependent] -> CombDependent
combineGroup = foldl combine NotDependent

combine :: CombDependent -> CombDependent -> CombDependent
combine a NotDependent = a
combine NotDependent b = b
combine (Combinatorial a) (Combinatorial b) = Combinatorial (List.nub (a ++ b))

dependencies :: CombDependent -> [String]
dependencies NotDependent = []
dependencies (Combinatorial s) = s

uniteDependentGroups :: [(a, CombDependent)] -> [([a], CombDependent)]
uniteDependentGroups [] = []
uniteDependentGroups (x : xs) = go (first List.singleton x) xs
  where
    go only [] = [only]
    go (a, c) xs
      | not $ null group = go this' rest
      | otherwise = this' : uniteDependentGroups xs
      where
        this = dependencies c
        this' = (a ++ as, combineGroup (c : combs))
        (group, rest) = List.partition (not . null . List.intersect this . dependencies . snd) xs
        (as, combs) = unzip group

-- | Mark groups of outputs depending on which inputs they depend on.
--
markDependentOutputs
  :: Module
  -- ^ Module to analyse
  -> [String]
  -- ^ Name of the inputs ports that are known to be independent from the outputs
  -> [(Port, CombDependent)]
markDependentOutputs Module{modInputs=inputs, modOutputs=outputs, modCells=cells} nonCombInputs = map lookupPort outputs
  where
    lookupWire :: Int -> IM.IntMap CombDependent -> CombDependent
    lookupWire = (fromMaybe NotDependent .) . IM.lookup

    lookupPort :: Port -> (Port, CombDependent)
    lookupPort p@Port{pBits=bits} = (p, comb)
      where
        comb = combineGroup . map (`lookupWire` finalMark) $ wires bits

    --TODO: make a better graph traversal than whatever I'm going to write now on March 26th 2025 at 14:25, and better than updated on March 31st at 14:03
    front = concatMap (wires . pBits) inputs
    initialMark = IM.fromList $ concatMap initMark inputs
      where
        initMark Port{pName = name, pBits = bits} =
          map (, if name `elem` nonCombInputs then NotDependent else Combinatorial [name]) (wires bits)
    finalMark = go front initialMark

    go :: [Int] -> IM.IntMap CombDependent -> IM.IntMap CombDependent
    go [] marking = marking
    go queue marking = go next (maybe marking (IM.unionWith combine marking) newMarksUnited)
      where
        visitCell Cell{cInputs=ins, cOutputs=outs}
          | (not . null) $ queue `List.intersect` inputs =
            let
              combs = combineGroup $ map (`lookupWire` marking) inputs
            in Just (outputs, IM.fromList $ map (, combs) outputs)
          | otherwise = Nothing
          where
            inputs = concatMap wires . Map.elems $ ins
            outputs = concatMap wires . Map.elems $ outs

        (nextCandidates, newMarks) = V.unzip $ V.mapMaybe visitCell cells
        newMarksUnited = uncurry (foldl (IM.unionWith combine)) <$> uncons (V.toList newMarks)
        next = concatMap (filter (\a -> IM.lookup a marking /= (IM.lookup a =<< newMarksUnited))) nextCandidates

newtype Domain = Domain Int

markDomains :: Module -> [(Port, Domain)]
markDomains Module{modInputs=inputs, modOutputs=outputs, modCells=cells} = undefined
