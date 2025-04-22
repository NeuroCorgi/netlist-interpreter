{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Clash.CoSim.Yosys.Internal
  ( Role(..)
  , markRolesForInputs
  , reorder
  -- * Dependent group detection
  , CombDependent(..)
  , dependencies
  , markDependentOutputs
  , uniteDependentGroups
  , splitModule
  )
where

import Control.Arrow (first, (&&&))

import Language.Haskell.TH hiding (Role)

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map (lookup, elems)
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)

import qualified Data.Vector as V

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
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
-- unfoldType :: Type -> Q ([(String, Role)], [String])
-- unfoldType (ForallT doms cxt arrow)
--   | null cxt = unfoldType arrow
--   | otherwise = error "expected no constraints"
-- unfoldType typ = do
--   let (argTys, retTy) = unsnoc $ NonEmpty.unfoldr (\case (AppT (AppT ArrowT arg) rest) -> (arg, Just rest); other -> (other, Nothing)) typ
--   argTyNames <- arrowNames argTys
--   retTuNames <- returnNames retTy
--   pure (argTyNames, [])
--   where
--     -- annotated name, not the real name of the type
--     typeNameRole (AppT (AppT (ConT tripleColon) (LitT (StrTyLit name))) ty)
--       | nameBase tripleColon == ":::" = pure (name, typeRole ty)
--       | otherwise = error "something else but triple colon encountered"
--     typeNameRole _ = error "something completely wrong encountered"
--
--     typeRole (ConT tyName)
--       | tyNameBase == "Clock" = Clock (Domain 0)
--       | tyNameBase == "Reset" = Reset (Domain 0)
--       | tyNameBase == "Enable" = Enable (Domain 0)
--       where
--         tyNameBase = nameBase tyName
--     typeRole (AppT (ConT tyName) arg)
--       | tyNameBase == "Clock" = Clock (Domain 1)
--       | tyNameBase == "Reset" = Reset (Domain 1)
--       | tyNameBase == "Enable" = Enable (Domain 1)
--       | tyNameBase == "Signal" = Signal (Domain 0) (tyBitVectorLen arg)
--       where
--         tyNameBase = nameBase tyName
--     typeRole (AppT (AppT (ConT tyName) (VarT domName)) arg)
--       | nameBase tyName == "Signal" = Signal (Domain 1) (tyBitVectorLen arg)
--     typeRole _ = error "something really wrong was passed"
--
--     tyBitVectorLen (AppT (ConT bitVectorName) (LitT (NumTyLit n)))
--       | nameBase bitVectorName == "BitVector" = fromInteger n
--     tyBitVectorLen _ = error "expected bitvector at some point"
--
--     arrowNames :: [Type] -> Q [(String, Role)]
--     arrowNames = mapM typeNameRole
--
--     returnNames :: Type -> Q [(String, Role)]
--     returnNames typ = mapM typeNameRole retTypes
--       where
--         retTypes = NonEmpty.toList $ NonEmpty.unfoldr (\case (AppT (TupleT _) arg) -> (arg, Nothing); (AppT rest arg) -> (arg, Just rest)) typ
--
--     -- taken from cabal-syntax Distribution.Utils.Generic
--     unsnoc :: NonEmpty.NonEmpty a -> ([a], a)
--     unsnoc (x NonEmpty.:| xs) = go x xs
--       where
--         go y [] = ([], x)
--         go y (z : zs) = let (ws, w) = go z zs in (y : ws, w)

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
      | like clock name = (p, dClock)
      | like reset name = (p, dReset)
      | like enable name = (p, dEnable)
    -- Second guess by loking at internal cell inputs
      | cs == cellClockInputs = (p, dClock)
      | cs == cellResetInputs = (p, dReset)
      | cs == cellEnableInputs = (p, dEnable)
      | otherwise = (p, Other) -- Value (length $ pBits p))
      where cs = wires bits

    dClock, dReset, dEnable :: Role
    dClock = Clock -- (Domain 0)
    dReset = Reset -- (Domain 0)
    dEnable = Enable -- (Domain 0)

    like f name = or $ f <*> return name

    cellInputs signal = List.nub . concatMap wires . V.mapMaybe (\Cell{cInputs=ins} -> Map.lookup signal ins)
    cellClockInputs = cellInputs "CLK" cells
    cellResetInputs = cellInputs "ARST" cells
    cellEnableInputs = cellInputs "EN" cells

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

data Origin
  = ModInput String
  | CellOut Int
  deriving Show

data ModuleSection = ModuleSection
  { msInputs :: S.Set String
  , msOutput :: String
  , msCells :: IS.IntSet
  }

emptyModuleSection :: String -> ModuleSection
emptyModuleSection output = ModuleSection S.empty output IS.empty

splitModule :: Module -> [String] -> [Module]
splitModule mod@Module{modInputs=inputs, modOutputs=outputs, modCells=cells} _ = mods
  where
    sourceOrigin :: IM.IntMap Origin
    sourceOrigin = IM.fromList (inputsOrigin ++ cellOutputsOrigin)

    inputsOrigin = concatMap ( uncurry (\s-> map (, ModInput s)) . (pName &&& wires . pBits)) inputs
    cellOutputsOrigin = concat $ V.toList $ V.imap (\i -> map (, CellOut i) . concatMap wires . Map.elems . cOutputs) cells

    mods = map (\Port{pName=name, pBits=bits} -> sectionToModule $ singleSection IS.empty (emptyModuleSection name) $ wires bits) outputs

    singleSection :: IS.IntSet -> ModuleSection -> [Int] -> ModuleSection
    singleSection _ modSect [] = modSect
    singleSection visited modSect outs = singleSection newVisited modSect' newOuts
      where
        (modSect', sources) = List.mapAccumL
          (\sect out ->
             case sourceOrigin IM.! out of
               CellOut i -> (sect{msCells=IS.insert i (msCells sect)}, Just (concatMap wires . Map.elems . cInputs $ cells V.! i))
               ModInput s -> (sect{msInputs=S.insert s (msInputs sect)}, Nothing)
          ) modSect outs

        newVisited = foldr IS.insert visited outs
        newOuts = concatMap (filter (not . (`IS.member` visited))) (catMaybes sources)

    sectionToModule :: ModuleSection -> Module
    sectionToModule ModuleSection{msInputs, msOutput, msCells} =
      let cs = trace ("Cells: " ++ show msCells) msCells
      in
      mod{ modInputs = filter ((`S.member` msInputs) . pName) inputs
         , modOutputs = filter ((== msOutput) . pName) outputs
         , modCells = V.fromList $ map (cells V.!) $ IS.elems cs }
