{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graph where

import Prelude hiding ((!!))

import Control.Arrow (first, second, (&&&), (***))
import Data.Foldable (foldlM)

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Maybe as Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Tuple (swap)

import Debug.Trace (trace, traceShowId, traceShow)

import MyLib

import Memory hiding (memory)
import Data.Function (on)

infixr 9 <.>
(<.>) :: (Functor m) => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g x = f <$> g x

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

maybeTyple :: (Maybe a, b) -> Maybe (a, b)
maybeTyple (Just a, b) = Just (a, b)
maybeTyple (Nothing, _) = Nothing

newtype Node = Node (Memory IntMap -> Memory IntMap, [Int], [Int])

inputs :: Node -> [Int]
inputs (Node (_, ins, _)) = ins

ofCell :: Map String b -> Cell -> Either String Node
ofCell subDesignMap Cell {..} =
  case cType of
    "$buf" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      y <- validateWidth width =<< lookup' "Y" outs
      Right (Node (id, wires a, wires y))
    "$logic_not" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((!|) ab yb, a, y))
    "$neg" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((-|) ab yb, a, y))
    "$not" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((~|) ab yb, a, y))
    "$pos" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((+|) ab yb, a, y))
    "$reduce_and" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((&/|) ab yb, a, y))
    "$reduce_bool" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((!!|) ab yb, a, y))
    "$reduce_or" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((^/|) ab yb, a, y))
    "$reduce_xnor" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((!^/|) ab yb, a, y))
    "$reduce_xor" -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((^/|) ab yb, a, y))
    "$add" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|+|) ab bb yb, a ++ b, y))
    "$and" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|&|) ab bb yb, a ++ b, y))
    "$mul" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|*|) ab bb yb, a ++ b, y))
    "$sub" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|-|) ab bb yb, a ++ b, y))
    "$bweqx" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node ((|===.|) (toBitVector (a, False)) (toBitVector (b, False)) (toBitVector (y, False)), wires a ++ wires b, wires y))
    "$div" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|//|) ab bb yb, a ++ b, y))
    "$divfloor" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|//|) ab bb yb, a ++ b, y))
    "$mod" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node (modB ab bb yb, a ++ b, y))
    "$modfloor" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node (modB ab bb yb, a ++ b, y))
    "$eq" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|==|) ab bb yb, a ++ b, y))
    "$eqx" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|===|) ab bb yb, a ++ b, y))
    "$ge" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|>=|) ab bb yb, a ++ b, y))
    "$gt" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|>|) ab bb yb, a ++ b, y))
    "$le" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|<=|) ab bb yb, a ++ b, y))
    "$lt" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|<|) ab bb yb, a ++ b, y))
    "$logic_and" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|&&|) ab bb yb, a ++ b, y))
    "$logic_or" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((||||) ab bb yb, a ++ b, y))
    "$or" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|||) ab bb yb, a ++ b, y))
    "$xor" -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|^|) ab bb yb, a ++ b, y))
    "$bmux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth (2 ^ s_width * width) =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (bmux (toBitVector a) (toBitVector s) (toBitVector y), wires a ++ wires s, wires y))
    "$bwmux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (bwmux (toBitVector a) (toBitVector b) (toBitVector s) (toBitVector y), wires a ++ wires b ++ wires s, wires y))
    "$demux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth (width * 2 ^ s_width) =<< lookup' "Y" outs
      return (Node
              (demux (toBitVector a) (toBitVector s) (toBitVector y), wires a ++ wires s, wires y))
    "$mux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth 1 =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (mux (toBitVector a) (toBitVector b) (toBitVector s) (toBitVector y), wires a ++ wires b ++ wires s, wires y))
    "$pmux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth (width * s_width) =<< lookup' "B" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (pmux (toBitVector a) (toBitVector b) (toBitVector s) (toBitVector y), wires a ++ wires b ++ wires s, wires y))
    "$tribuf" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (tribuf (toBitVector a) (toBitVector en) (toBitVector y), wires a ++ wires en, wires y))
    "$adff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol<- bitString <$> lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (adff (V.head clkPol) (V.head rstPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d, wires q))
    "$adffe" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      enPol <- bitString <$> lookup' "EN_POLARITY" cParameters
      rstPol<- bitString <$> lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (adffe (V.head clkPol) (V.head rstPol) (V.head enPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector en) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires en ++ wires d, wires q))
    "$adlatch" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" cParameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (adlatch (V.head enPol) (V.head rstPol) (toBitVector rstVal) (toBitVector en) (toBitVector rst) (toBitVector d) (toBitVector q), wires en ++ wires rst ++ wires d, wires q))
    "$aldff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ALOAD_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (aldff (V.head clkPol) (V.head rstPol) (toBitVector clk) (toBitVector rst) (toBitVector ad) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d ++ wires ad, wires q))
    "$aldffe" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ALOAD_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (aldffe (V.head clkPol) (V.head rstPol) (V.head enPol) (toBitVector clk) (toBitVector rst) (toBitVector en) (toBitVector ad) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires en ++ wires d ++ wires ad, wires q))
    "$dff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (dff (V.head clkPol) (toBitVector clk) (toBitVector d) (toBitVector q), wires clk ++ wires d, wires q))
    "$dffe" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (dffe (V.head clkPol) (V.head enPol) (toBitVector clk) (toBitVector en) (toBitVector d) (toBitVector q), wires clk ++ wires en ++ wires d, wires q))
    "$sdff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol <- bitString <$> lookup' "SRST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "SRST_VALUE" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "SRST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (sdff (V.head clkPol) (V.head rstPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d, wires q))
    other
      | Just subdesign <- Map.lookup other subDesignMap ->
          undefined
      | otherwise -> Left ("Unknown node: " ++ other)
  where
    num :: String -> Either String Int
    num = foldlM (\y x -> (+) <$> d2i x <*> return (y * 2)) 0
    d2i :: Char -> Either String Int
    d2i '0' = Right 0
    d2i '1' = Right 1
    d2i  _  = Left ""

    validateWidth :: Foldable t => Int -> t a -> Either String (t a)
    validateWidth n v
      | length v == n = Right v
      | otherwise = Left ("inconsistent width: expected " ++ show n ++ ", got " ++ show (length v))

    lookup' :: (Show a, Ord a) => a -> Map a b -> Either String b
    lookup' key map =
      case Map.lookup key map of
        Just a -> Right a
        Nothing -> Left ("Not found key: " ++ show key)

    unaryMath :: Either String ([Int], [Int], BitVector, BitVector)
    unaryMath = do
      a_signed <- (== 1) <$> (num =<< lookup' "A_SIGNED" cParameters)
      a_width <- num =<< lookup' "A_WIDTH" cParameters
      y_width <- num =<< lookup' "Y_WIDTH" cParameters
      a <- validateWidth a_width =<< lookup' "A" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      Right (wires a, wires y, toBitVector (a, a_signed), toBitVector y)

    binaryMath :: Either String ([Int], [Int], [Int], BitVector, BitVector, BitVector)
    binaryMath = do
      a_signed <- (== 1) <$> (num =<< lookup' "A_SIGNED" cParameters)
      b_signed <- (== 1) <$> (num =<< lookup' "B_SIGNED" cParameters)
      a_width <- num =<< lookup' "A_WIDTH" cParameters
      b_width <- num =<< lookup' "B_WIDTH" cParameters
      y_width <- num =<< lookup' "Y_WIDTH" cParameters
      a <- validateWidth a_width =<< lookup' "A" ins
      b <- validateWidth b_width =<< lookup' "B" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      Right (wires a, wires b, wires y, toBitVector (a, a_signed), toBitVector (b, b_signed), toBitVector y)

    (ins, outs) = both Map.fromList . L.partition ((fromMaybe False . (== Input) <.> (`Map.lookup` cPortDirections)) . fst) $ Map.toList cConnections

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

-- compileSub :: Module -> Either String (SubDesign, [(BitVector, [Bit])]
-- compileSub Module{..} = do
  -- nodes <- mapM ofCell cells
  -- let update_map = foldl (\m (key, val) -> Map.insertWith (++) key [val] m) Map.empty . concatMap (\(i, (_, nodeIns, _)) -> map (, i) nodeIns) $ zip [0..] nodes

compile :: NonEmpty Module -> Either String Design
compile (Module {cells=mCells, ports, netnames} :| submods) = do
  nodes' <- nodes
  let updateMap = foldl (\m (key, val) -> Map.insertWith (++) key [val] m) Map.empty . concatMap (\(i, (Node (_, nodeIns, _))) -> map (, i) nodeIns) $ zip [0..] nodes'
  return Design
    { dNodes = nodes'
    , dIns = ins
    , dOuts = outs
    , dMemory = foldl (//) (empty (n_bits (ports, netnames))) inits
    , dUpdateMap = updateMap
    }
  where
    -- submods = topoSort submods'
    nodes = mapM (ofCell Map.empty) mCells

    n_bits = uncurry max . (pb *** nb)
    pb = maximum' . map (maximum' . wires . pBits)
    nb = maximum' . map (maximum' . wires . nBits)

    maximum' [] = 0
    maximum' l@(_:_) = maximum l

    inits = mapMaybe (\Net{..} -> (toBitVector nBits, ) . bitString <$> Map.lookup "init" nAttributes) netnames

    ins = map (pName &&& (toBitVector . pBits)) $ filter ((== Input) . direction) ports
    outs = map (pName &&& (toBitVector . pBits)) $ filter ((== Output) . direction) ports

step :: Design -> Design
step d@Design {dMemory, dNodes, dUpdateMap} = d{dMemory=go dMemory}
  where
    go mem@Memory{mUpdated=[]} = mem
    go oldMem@Memory{mUpdated=updates} = newMem
    -- The first update block should persist to allow to detect *all* edges
      where
        ups = concat updates
        influencedNodesInds = L.nub $ concat $ mapMaybe ((`Map.lookup` dUpdateMap) . fst) ups
        influencedNodes = map (dNodes L.!!) influencedNodesInds

        newMem = melt oldMem $ map (\(Node (f, ins, outs)) -> (, outs) $ f $ freeze oldMem (ins ++ outs)) influencedNodes

        freeze :: Memory Vector -> [Int] -> Memory IntMap
        freeze mem@Memory{..} freezeInds = Memory
          { mMemory=IntMap.fromList $ map (id &&& (mMemory !!)) freezeInds
          , mUpdated=[ups] }

        melt :: Memory Vector -> [(Memory IntMap, [Int])] -> Memory Vector
        melt (Memory{mMemory=origMem}) cellMem = Memory
          -- Potential optimization. What is more efficient traverse `origMem` $n$ times, or sort $n$ lists shorter than `origMem`
          -- { mMemory = foldl (\om p -> patch p (zip [0,1..] om)) origMem (map (IntMap.toAscList . mMemory) cellMem)
          { mMemory = origMem V.// cellMemPatch
          -- Tail is needed to cut the first processed update
          , mUpdated = concatMap (init' . mUpdated . fst) cellMem }
          where
            cellMemPatch = L.sortBy (compare `on` fst) $ concatMap (\(m, i) -> map (id &&& (IntMap.!) (mMemory m)) i) cellMem

            init' [] = []
            init' l = init l

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
