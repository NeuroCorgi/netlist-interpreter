{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graph where

import Prelude hiding ((!!))

import Control.Arrow (first, second, (&&&))
import Data.Foldable (foldlM)

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Maybe as Maybe

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Tuple (swap)

import MyLib
import CellKinds

import Memory
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
  case cKind of
    Buf -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      y <- validateWidth width =<< lookup' "Y" outs
      Right (Node (id, wires a, wires y))
    LogicNot -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((!|) ab yb, a, y))
    Neg -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((-|) ab yb, a, y))
    Not -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((~|) ab yb, a, y))
    Pos -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((+|) ab yb, a, y))
    ReduceAnd -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((&/|) ab yb, a, y))
    ReduceBool -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((!!|) ab yb, a, y))
    ReduceOr -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((^/|) ab yb, a, y))
    ReduceXnor -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((!^/|) ab yb, a, y))
    ReduceXor -> do
      (a, y, ab, yb) <- unaryMath
      return (Node ((^/|) ab yb, a, y))
    Add -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|+|) ab bb yb, a ++ b, y))
    And -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|&|) ab bb yb, a ++ b, y))
    Mul -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|*|) ab bb yb, a ++ b, y))
    Sub -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|-|) ab bb yb, a ++ b, y))
    Bweqx -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node ((|===.|) (toBitVector (a, False)) (toBitVector (b, False)) (toBitVector (y, False)), wires a ++ wires b, wires y))
    Div -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|//|) ab bb yb, a ++ b, y))
    Divfloor -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|//|) ab bb yb, a ++ b, y))
    Mod -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node (modB ab bb yb, a ++ b, y))
    Modfloor -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node (modB ab bb yb, a ++ b, y))
    Eq -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|==|) ab bb yb, a ++ b, y))
    Eqx -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|===|) ab bb yb, a ++ b, y))
    Ge -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|>=|) ab bb yb, a ++ b, y))
    Gt -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|>|) ab bb yb, a ++ b, y))
    Le -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|<=|) ab bb yb, a ++ b, y))
    Lt -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|<|) ab bb yb, a ++ b, y))
    LogicAnd -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|&&|) ab bb yb, a ++ b, y))
    LogicOr -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((||||) ab bb yb, a ++ b, y))
    Or -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|||) ab bb yb, a ++ b, y))
    Xor -> do
      (a, b, y, ab, bb, yb) <- binaryMath
      return (Node ((|^|) ab bb yb, a ++ b, y))
    Bmux -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth (2 ^ s_width * width) =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (bmux (toBitVector a) (toBitVector s) (toBitVector y), wires a ++ wires s, wires y))
    Bwmux -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (bwmux (toBitVector a) (toBitVector b) (toBitVector s) (toBitVector y), wires a ++ wires b ++ wires s, wires y))
    Demux -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth (width * 2 ^ s_width) =<< lookup' "Y" outs
      return (Node
              (demux (toBitVector a) (toBitVector s) (toBitVector y), wires a ++ wires s, wires y))
    Mux -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth 1 =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (mux (toBitVector a) (toBitVector b) (toBitVector s) (toBitVector y), wires a ++ wires b ++ wires s, wires y))
    Pmux -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth (width * s_width) =<< lookup' "B" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (pmux (toBitVector a) (toBitVector b) (toBitVector s) (toBitVector y), wires a ++ wires b ++ wires s, wires y))
    Tribuf -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node
              (tribuf (toBitVector a) (toBitVector en) (toBitVector y), wires a ++ wires en, wires y))
    Adff -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol<- bitString <$> lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (adff (head clkPol) (head rstPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d, wires q))
    Adffe -> do
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
              (adffe (head clkPol) (head rstPol) (head enPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector en) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires en ++ wires d, wires q))
    Adlatch -> do
      width <- num =<< lookup' "WIDTH" cParameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" cParameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (adlatch (head enPol) (head rstPol) (toBitVector rstVal) (toBitVector en) (toBitVector rst) (toBitVector d) (toBitVector q), wires en ++ wires rst ++ wires d, wires q))
    Aldff -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ALOAD_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (aldff (head clkPol) (head rstPol) (toBitVector clk) (toBitVector rst) (toBitVector ad) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d ++ wires ad, wires q))
    Aldffe -> do
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
              (aldffe (head clkPol) (head rstPol) (head enPol) (toBitVector clk) (toBitVector rst) (toBitVector en) (toBitVector ad) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires en ++ wires d ++ wires ad, wires q))
    Dff -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (dff (head clkPol) (toBitVector clk) (toBitVector d) (toBitVector q), wires clk ++ wires d, wires q))
    Dffe -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (dffe (head clkPol) (head enPol) (toBitVector clk) (toBitVector en) (toBitVector d) (toBitVector q), wires clk ++ wires en ++ wires d, wires q)) 
    Sdff -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol <- bitString <$> lookup' "SRST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "SRST_VALUE" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "SRST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (sdff (head clkPol) (head rstPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d, wires q))
    SubDesign name
      | Just subdesign <- Map.lookup name subDesignMap ->
          undefined
      | otherwise -> Left ("Unknown node: " ++ name)
  where
    num :: String -> Either String Int
    num = foldlM (\y x -> (+) <$> d2i x <*> return (y * 2)) 0
    d2i :: Char -> Either String Int
    d2i '0' = Right 0
    d2i '1' = Right 1
    d2i  _  = Left ""

    validateWidth :: Int -> [a] -> Either String [a]
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
  , dMemory :: Memory []
  , dUpdateMap :: Map Int [Int]
  }

instance Show Design where
  show Design{..} =
    "Design{ins=[" ++ show dIns ++ "], outs=[" ++ show dOuts ++ "], update_map=" ++ show dUpdateMap ++ ", memory=" ++ show dMemory ++ "}"

compile :: NonEmpty Module -> Either String Design
compile (Module {modCells=cells, modInputs=inputs, modOutputs=outputs, modNetnames=netnames} :| submods) = do
  nodes' <- nodes
  let updateMap = foldl (\m (key, val) -> Map.insertWith (++) key [val] m) Map.empty . concatMap (\(i, Node (_, nodeIns, _)) -> map (, i) nodeIns) $ zip [0..] nodes'
  return $ eval $ Design
    { dNodes = nodes'
    , dIns = ins
    , dOuts = outs
    , dMemory = foldl (//) (empty (n_bits inputs outputs netnames)) inits
    , dUpdateMap = updateMap
    }
  where
    -- submods = topoSort submods'
    nodes = mapM (ofCell Map.empty) cells

    n_bits ins outs nets = maximum' [pb ins, pb outs, nb nets]
    pb = maximum' . map (maximum' . wires . pBits)
    nb = maximum' . map (maximum' . wires . nBits)

    maximum' [] = 0
    maximum' l@(_:_) = maximum l

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
          , mUpdated=[update] }

        melt :: Memory [] -> [(Memory IntMap, [Int])] -> Memory []
        melt (Memory{mMemory=origMem}) cellMem = Memory
          -- Potential optimization. What is more efficient traverse `origMem` $n$ times, or sort $n$ lists shorter than `origMem`
          -- { mMemory = foldl (\om p -> patch p (zip [0,1..] om)) origMem (map (IntMap.toAscList . mMemory) cellMem)
          { mMemory = patch cellMemPatch (zip [0,1..] origMem)
          -- Tail is needed to cut the first processed update
          , mUpdated = rest ++ concatMap (tail' . mUpdated . fst) cellMem }
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
