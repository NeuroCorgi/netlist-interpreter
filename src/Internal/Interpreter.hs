{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}

module Internal.Interpreter where

import Prelude hiding ((!!))

import Control.Arrow (first, second, (&&&))

import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Maybe as Maybe

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Tuple (swap)
import Data.Function (on)

import Internal.Intermediate
import Internal.Memory
import Internal.Interpreter.Node
import Internal.Interpreter.CompState

maybeTyple :: (Maybe a, b) -> Maybe (a, b)
maybeTyple (Just a, b) = Just (a, b)
maybeTyple (Nothing, _) = Nothing

data Design = Design
  { dNodes :: [Node]
  , dIns :: [(String, BitVector)]
  , dOuts :: [(String, BitVector)]
  , dMemory :: Memory []
  , dUpdateMap :: Map Int [Int]
  }

instance Show Design where
  show Design{..} =
    "Design{ins=[" ++ show dIns ++ "], outs=[" ++ show dOuts ++ "], update_map=" ++ show dUpdateMap ++ ", memory=" ++ show dMemory ++ "}"

compile :: Module -> Either String Design
compile topLevel@Module{modCells=cells, modInputs=inputs, modOutputs=outputs, modNetnames=netnames} = do
  (nodes, submem, subdes) <- state
  let updateMap = foldl (\m (key, val) -> Map.insertWith (++) key [val] m) Map.empty . concatMap (\(i, Node (_, nodeIns, _)) -> map (, i) nodeIns) $ zip [0..] nodes
  -- First eval to propagate initial values
  return $ eval $ Design
    { dNodes = nodes
    , dIns = ins
    , dOuts = outs
    , dMemory = foldl (//) (empty memorySize submem subdes) inits
    , dUpdateMap = updateMap
    }
  where
    state = evalDesignState $ mapM (ofCell Map.empty) cells

    memorySize = modMemorySize topLevel

    inits = mapMaybe (\Net{..} -> (toBitVector nBits, ) . bitString <$> Map.lookup "init" nAttributes) netnames

    ins = map (pName &&& (toBitVector . pBits)) inputs
    outs = map (pName &&& (toBitVector . pBits)) outputs

step :: Design -> Design
step d@Design {dMemory, dNodes, dUpdateMap} = d{dMemory=go dMemory}
  where
    go mem@Memory{mUpdated=[]} = mem
    go oldMem@Memory{mUpdated=update:rest} = newMem
    -- The first update block should persist to allow to detect *all* edges
      where
        influencedNodesInds = L.nub . concat $ mapMaybe ((`Map.lookup` dUpdateMap) . fst) update
        influencedNodes = map (dNodes !!) influencedNodesInds

        newMem = melt oldMem $ map (\(Node (f, ins, outs)) -> (, outs) $ f $ freeze oldMem (ins ++ outs)) influencedNodes

        freeze :: Memory [] -> [Int] -> Memory IntMap
        freeze mem@Memory{..} freezeInds = Memory
          { mMemory=IntMap.fromList $ map (id &&& (mMemory !!)) freezeInds
          , mUpdated=[update]
          , mSubMem = mempty
          , mSubDes = mempty }

        melt :: Memory [] -> [(Memory IntMap, [Int])] -> Memory []
        melt (Memory{mMemory=origMem}) cellMem = Memory
          -- Potential optimization. What is more efficient traverse `origMem` $n$ times, or sort $n$ lists shorter than `origMem`
          -- { mMemory = foldl (\om p -> patch p (zip [0,1..] om)) origMem (map (IntMap.toAscList . mMemory) cellMem)
          { mMemory = patch cellMemPatch (zip [0,1..] origMem)
          -- Tail is needed to cut the first processed update
          , mUpdated = rest ++ concatMap (tail' . mUpdated . fst) cellMem
          , mSubMem = mempty
          , mSubDes = mempty }
          where
            cellMemPatch = L.sortBy (compare `on` fst) $ concatMap (\(m, i) -> map (id &&& (IntMap.!) (mMemory m)) i) cellMem
            patch b@((i, e) : r) l@((j, n) : t)
              | i < j = patch r l
              | i == j = e : patch r t
              | i > j = n : patch b t
            patch _ l = map snd l

            tail' [] = []
            tail' (_:t) = t

eval :: Design -> Design
eval d@Design{dMemory=Memory{mUpdated=[]}} = d
eval d = eval . step $ d

start :: Design -> Map String Integer -> Design
start d@Design {dMemory=mem, dIns, dOuts} state =
  let inits = Maybe.mapMaybe (swap <.> maybeTyple . first (`Map.lookup` state)) dIns
      memory' = L.foldl' (\m (a, n) -> m // (a, extend (bvLength a) $ fromInt n)) mem inits
  in
      d {dMemory = memory'}

peek :: Design -> Map String (Maybe Integer)
peek Design{dMemory, dOuts} = Map.fromList $ map (second (toInt . (dMemory !))) dOuts

exec :: Design -> Map String Integer -> Map String (Maybe Integer)
exec d@Design {dMemory=mem, dIns, dOuts} state =
  let inits = Maybe.mapMaybe (swap <.> maybeTyple . first (`Map.lookup` state)) dIns
      memory' = L.foldl' (\m (a, n) -> m // (a, fromInt n)) mem inits
      d' = eval d {dMemory = memory'}
      memory'' = dMemory d'
   in Map.fromList $ map (second (toInt . (memory'' !))) dOuts
