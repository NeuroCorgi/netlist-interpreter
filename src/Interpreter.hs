{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}

module Interpreter where

import Prelude hiding ((!!))

import Control.Arrow (first, second, (&&&))

import Data.Vector (Vector)
import qualified Data.List as L

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (mapMaybe)
import qualified Data.Maybe as Maybe

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Tuple (swap)
import Data.Function (on)

import Intermediate
import Memory
import Interpreter.Node
import Interpreter.CompState

maybeTyple :: (Maybe a, b) -> Maybe (a, b)
maybeTyple (Just a, b) = Just (a, b)
maybeTyple (Nothing, _) = Nothing

data Design = Design
  { dNodes :: [Node]
  , dIns :: [(String, BitVector)]
  , dOuts :: [(String, BitVector)]
  , dMemory :: Memory Vector
  , dUpdateMap :: Map Int [Int]
  }

instance Show Design where
  show Design{..} =
    "Design{ins=[" ++ show dIns ++ "], outs=[" ++ show dOuts ++ "], update_map=" ++ show dUpdateMap ++ ", memory=" ++ show dMemory ++ "}"

compile :: Module -> Either String Design
compile topLevel@Module{modCells=cells, modInputs=inputs, modOutputs=outputs, modNetnames=netnames} = do
  (nodes, submem, subdes) <- state
  -- a map of wires causing updates to nodes.
  --TODO: possibly optimize lists
  -- it's not a performance bottleneck, so let it be for now
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

    -- how many wires to be allocated
    memorySize = modMemorySize topLevel

    -- init values for some of the connections, they would be set and propagated before the first clock cycle
    inits = mapMaybe (\Net{..} -> (toBitVector nBits, ) . bitString <$> Map.lookup "init" nAttributes) netnames

    ins = map (pName &&& (toBitVector . pBits)) inputs
    outs = map (pName &&& (toBitVector . pBits)) outputs

step :: Design -> Design
--TODO: maximum amount of steps?
step d@Design {dMemory, dNodes, dUpdateMap} = d{dMemory=go dMemory}
  where
    go mem@Memory{mUpdated=[]} = mem
    go oldMem@Memory{mUpdated=updates} = newMem
      where
        -- Updates needs to be concatenated, so all the changes are present at once, and edge detection works correctly
        ups = concat updates
        -- `nub` here should not cause much performance problems
        -- list of cells to update consist of largely consequent identical elements (i.e. [1,1,1,2,2,3,3,3])
        -- therefore `nub` would have its best case time complexity.
        -- Although, a something else might be better, having that we have known the exact number of cells and ideally
        -- we can check if node was seen in constant time
        influencedNodesInds = L.nub . concat $ mapMaybe ((`Map.lookup` dUpdateMap) . fst) ups
        influencedNodes = map (dNodes L.!!) influencedNodesInds

        runningMem = stageUpdates oldMem
        endMem@Memory{mUpdated=newUpdates} = L.foldl' (\mem (Node (f, _, _)) -> f mem) runningMem influencedNodes
        newMem = endMem{mUpdated=drop (length updates) newUpdates}

-- | Evaluates a design in whatever state it might currently be
eval :: Design -> Design
-- evaluates till there are no more updates left
-- let's hope that it always terminates
eval d@Design{dMemory=Memory{mUpdated=[]}} = d
eval d = eval . step $ d

-- | Set inputs of a design
--   if input with a provided name does not exist, it is ignored
start ::
  Design ->
  -- ^ Design to set inputs in
  Map String Integer
  -- ^ Map of inputs to set. Keys are name of the inputs, values are values and are converted to bitvectors internally
  -> Design
  -- ^ A new design with new inputs set
start d@Design {dMemory=mem, dIns} state =
  let inits = Maybe.mapMaybe (swap <.> maybeTyple . first (`Map.lookup` state)) dIns
      memory' = L.foldl' (\m (a, n) -> m // (a, extend (bvLength a) $ fromInt n)) mem inits
  in
      d {dMemory = memory'}

-- | Probe output values at the current state of the design
peek ::
  Design ->
  Map String (Maybe Integer)
  -- ^ `Nothing` for an output represent presence of undefined or high-impedence bits in the output bitvector
peek Design{dMemory, dOuts} = Map.fromList $ map (second (toInt . (dMemory !))) dOuts

-- | Given the inputs, gets the outputs of the design.
--   To get resulting state of the design, see `start` and `eval`
exec ::
  Design ->
  Map String Integer ->
  Map String (Maybe Integer)
  -- ^ See `peek` for more information about outputs
exec = ((peek . eval) .) . start -- B1 or blackbird
