{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Graph where

import Control.Arrow (first, second, (&&&), (***))
import Data.Foldable (foldlM)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Tuple (swap)

import Debug.Trace (trace, traceShowId, traceShow)

import MyLib

import Memory hiding (memory)

infixr 9 <.>
(<.>) :: (Functor m) => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g x = f <$> g x

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

zipLeftLongest :: [a] -> [b] -> b -> [(a, b)]
zipLeftLongest (a : ra) (b : rb) d = (a, b) : zipLeftLongest ra rb d
zipLeftLongest (a : ra) [] d = (a, d) : zipLeftLongest ra [] d
zipLeftLongest [] _ _ = []

maybeTyple :: (Maybe a, b) -> Maybe (a, b)
maybeTyple (Just a, b) = Just (a, b)
maybeTyple (Nothing, _) = Nothing

type Node = (Memory -> Memory, [Int], [Int])

inputs :: Node -> [Int]
inputs (_, ins, _) = ins

ofCell :: Map String SubDesign -> Cell -> Either String Node
ofCell subDesignMap Cell {..} =
  case cType of
    "$buf" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      y <- validateWidth width =<< lookup' "Y" outs
      Right (id, wires a, wires y)
    "$logic_not" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((!|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$neg" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((-|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$not" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((~|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$pos" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((+|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_and" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((&/|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_bool" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((!!|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_or" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((^/|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_xnor" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((!^/|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_xor" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((^/|) ab yb, a, y)
        -- Signed
        _ -> Left "unsupported"
    "$add" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|+|) ab bb yb, a ++ b, y)
    "$and" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|&|) ab bb yb, a ++ b, y)
    "$bweqx" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      y <- validateWidth width =<< lookup' "Y" outs
      Right ((|===.|) (BitVector a) (BitVector b) (BitVector y), wires a ++ wires b, wires y)
    "$div" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|//|) ab bb yb, a ++ b, y)
    "$divfloor" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported" -- Do some other division here
        _ -> Right ((|//|) ab bb yb, a ++ b, y)
    "$mod" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right (modB ab bb yb, a ++ b, y)
    "$modfloor" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported" -- Do some other modision here
        _ -> Right (modB ab bb yb, a ++ b, y)
    "$eq" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|==|) ab bb yb, a ++ b, y)
    "$eqx" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|===|) ab bb yb, a ++ b, y)
    "$ge" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|>=|) ab bb yb, a ++ b, y)
    "$gt" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|<|) ab bb yb, a ++ b, y)
    "$le" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|<=|) ab bb yb, a ++ b, y)
    "$lt" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|<|) ab bb yb, a ++ b, y)
    "$logical_and" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|&&|) ab bb yb, a ++ b, y)
    "$logical_or" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((||||) ab bb yb, a ++ b, y)
    "$bmux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth (2 ^ s_width * width) =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (bmux (BitVector a) (BitVector s) (BitVector y), wires a ++ wires s, wires y)
    "$bwmux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (bwmux (BitVector a) (BitVector b) (BitVector s) (BitVector y), wires a ++ wires b ++ wires s, wires y)
    "$demux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth (width * 2 ^ s_width) =<< lookup' "Y" outs
      return
        (demux (BitVector a) (BitVector s) (BitVector y), wires a ++ wires s, wires y)
    "$mux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth 1 =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (mux (BitVector a) (BitVector b) (BitVector s) (BitVector y), wires a ++ wires b ++ wires s, wires y)
    "$pmux" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      s_width <- num =<< lookup' "S_WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth (width * s_width) =<< lookup' "B" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (pmux (BitVector a) (BitVector b) (BitVector s) (BitVector y), wires a ++ wires b ++ wires s, wires y)
    "$tribuf" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (tribuf (BitVector a) (BitVector en) (BitVector y), wires a ++ wires en, wires y)
    "$adff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol<- bitString <$> lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (adff (head clkPol) (head rstPol) (bitVector rstVal) (BitVector clk) (BitVector rst) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires d, wires q)
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
      return
        (adffe (head clkPol) (head rstPol) (head enPol) (bitVector rstVal) (BitVector clk) (BitVector rst) (BitVector en) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires en ++ wires d, wires q)
    "$adlatch" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" cParameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (adlatch (head enPol) (head rstPol) (bitVector rstVal) (BitVector en) (BitVector rst) (BitVector d) (BitVector q), wires en ++ wires rst ++ wires d, wires q)
    "$aldff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ALOAD_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (aldff (head clkPol) (head rstPol) (BitVector clk) (BitVector rst) (BitVector ad) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires d ++ wires ad, wires q)
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
      return
        (aldffe (head clkPol) (head rstPol) (head enPol) (BitVector clk) (BitVector rst) (BitVector en) (BitVector ad) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires en ++ wires d ++ wires ad, wires q)
    "$dff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (dff (head clkPol) (BitVector clk) (BitVector d) (BitVector q), wires clk ++ wires d, wires q)
    "$dffe" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" cParameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (dffe (head clkPol) (head enPol) (BitVector clk) (BitVector en) (BitVector d) (BitVector q), wires clk ++ wires en ++ wires d, wires q)
    "$sdff" -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol <- bitString <$> lookup' "SRST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "SRST_VALUE" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "SRST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (sdff (head clkPol) (head rstPol) (bitVector rstVal) (BitVector clk) (BitVector rst) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires d, wires q)
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

    validateWidth :: Int -> [a] -> Either String [a]
    validateWidth n v
      | length v == n = Right v
      | otherwise = Left ("inconsistent width: expected " ++ show n ++ ", got " ++ show (length v))

    lookup' :: (Show a, Ord a) => a -> Map a b -> Either String b
    lookup' key map =
      case Map.lookup key map of
        Just a -> Right a
        Nothing -> Left ("Not found key: " ++ show key)

    unaryMath :: Either String (Int, [Int], [Int], BitVector, BitVector)
    unaryMath = do
      a_signed <- num =<< lookup' "A_SIGNED" cParameters
      a_width <- num =<< lookup' "A_WIDTH" cParameters
      y_width <- num =<< lookup' "Y_WIDTH" cParameters
      a <- validateWidth a_width =<< lookup' "A" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      Right (a_signed, wires a, wires y, BitVector a, BitVector y)

    binaryMath :: Either String (Int, Int, [Int], [Int], [Int], BitVector, BitVector, BitVector)
    binaryMath = do
      a_signed <- num =<< lookup' "A_SIGNED" cParameters
      b_signed <- num =<< lookup' "B_SIGNED" cParameters
      a_width <- num =<< lookup' "A_WIDTH" cParameters
      b_width <- num =<< lookup' "B_WIDTH" cParameters
      y_width <- num =<< lookup' "Y_WIDTH" cParameters
      a <- validateWidth a_width =<< lookup' "A" ins
      b <- validateWidth b_width =<< lookup' "B" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      Right (a_signed, b_signed, wires a, wires b, wires y, BitVector a, BitVector b, BitVector y)

    (ins, outs) = both Map.fromList . L.partition ((fromMaybe False . (== Input) <.> (`Map.lookup` cPortDirections)) . fst) $ Map.toList cConnections

data Design = Design
  { nodes :: [Node]
  , ins :: [(String, BitVector)]
  , outs :: [(String, BitVector)]
  , memory :: Memory
  , update_map :: Map Int [Int]
  }

data SubDesign = SubDesign
  { nodes :: [Node]
  , ins :: [(String, BitVector)]
  , outs :: [(String, BitVector)]
  , update_map :: Map Int [Int]
  }

instance Show Design where
  show Design{..} =
    "Design{ins=[" ++ show ins ++ "], outs=[" ++ show outs ++ "], update_map=" ++ show update_map ++ ", memory=" ++ show memory ++ "}"

-- compileSub :: Module -> Either String (SubDesign, [(BitVector, [Bit])]
-- compileSub Module{..} = do
  -- nodes <- mapM ofCell cells
  -- let update_map = foldl (\m (key, val) -> Map.insertWith (++) key [val] m) Map.empty . concatMap (\(i, (_, nodeIns, _)) -> map (, i) nodeIns) $ zip [0..] nodes

compile :: NonEmpty Module -> Either String Design
compile (Module {cells=mCells, ports, netnames} :| submods') = do
  nodes <- mapM (ofCell Map.empty) mCells
  let updateMap = foldl (\m (key, val) -> Map.insertWith (++) key [val] m) Map.empty . concatMap (\(i, (_, nodeIns, _)) -> map (, i) nodeIns) $ zip [0..] nodes
  return Design
    { nodes = nodes
    , ins = ins
    , outs = outs
    , memory = foldl (//) (empty (n_bits (ports, netnames))) inits
    , update_map = updateMap
    }
  where
    submods = topoSort submods'

    n_bits = uncurry max . (pb *** nb)
    pb = maximum' . map (maximum' . wires . pBits)
    nb = maximum' . map (maximum' . wires . nBits)

    maximum' [] = 0
    maximum' l@(_:_) = maximum l

    inits = mapMaybe (\Net{..} -> (BitVector nBits, ) . bitString <$> Map.lookup "init" nAttributes) netnames

    ins = map (pName &&& (BitVector . pBits)) $ filter ((== Input) . direction) ports
    outs = map (pName &&& (BitVector . pBits)) $ filter ((== Output) . direction) ports

    remap :: Int -> Module -> (Module, Int)
    remap shift m@Module{..} = (
      -- Possible to optimize for a single pass, to update and find next shift number at the same time
      m{ ports = map remapPort ports
       , cells = map remapCells cells
       , netnames = map remapNet netnames
       }, shift + n_bits (ports, netnames))
      where
        rewire = map (\case W a -> W (a + shift); b@(B _) -> b)

        remapPort p@Port{..} = p{ pBits = rewire pBits }
        remapCells c@Cell{..} = c{ cConnections = Map.map rewire cConnections}
        remapNet n@Net{..} = n{ nBits = rewire nBits }

    topoSort :: [Module] -> [Module]
    topoSort modules =
      map (nameMod Map.!) $ go [] Set.empty $ Map.keys nameMod
      where
        nameMod = Map.fromList $ map (name &&& id) modules
        referencedModules = Map.fromList $ map (name &&& map cType . filter (\case Cell{cType='$':_} -> False; _ -> True) . cells) modules
        
        go :: [String] -> Set String -> [String] -> [String]
        go sorted _seen [] = sorted
        go sorted seen queue = go (sorted ++ front) seen' queue'
          where
            (front, queue') = L.partition (all (`Set.member` seen) . (referencedModules Map.!)) queue
            seen' = foldl (\s' -> (`Set.insert` s')) seen front

step :: Design -> Design
step d@Design {memory, nodes, update_map} = d{memory=go memory}
  where
    go mem@Memory{updated=[]} = mem
    go oldMem@Memory{updated=update:_} = newMem
    -- The first update block should persist to allow to detect *all* edges
      where
        -- runningMem = oldMem{updated=ups}
        influencedNodesInds = traceShowId (L.nub . concat $ mapMaybe ((`Map.lookup` update_map) . fst) update)
        influencedNodes = map (\(a, _, _) -> a) . reverse . topoSort . prepare $ map (nodes !!) influencedNodesInds
        endMem@Memory{updated=_:rest} = traceShow oldMem $ traceShowId (foldl (\mem node -> node mem) oldMem influencedNodes)
        newMem = endMem{updated=rest}

        -- Trim all dangling inputs to prepare for topological sort
        -- Leave only inputs that are outputs of some node that we are sorting
        prepare :: [Node] -> [Node]
        prepare nodes = nodes'
          where
            outputs = IntSet.fromList $ concatMap (\(_, _, a) -> a) nodes
            nodes' = map (\(f, ins, outs) -> (f, filter (`IntSet.member` outputs) ins, outs)) nodes

        topoSort :: [Node] -> [Node]
        topoSort [] = []
        -- Stupid workaround for a self recursive cell
        topoSort [one] = [one]
        topoSort lst = go [] IntSet.empty lst
          where
            go :: [Node] -> IntSet -> [Node] -> [Node]
            go sorted _seen [] = sorted
            go sorted seen queue = go (sorted ++ front) seen' queue'
              where
                (front, queue') = L.partition (all (`IntSet.member` seen) . (\(_, a, _) -> a)) queue
                seen' = foldl (foldl (\s' -> (`IntSet.insert` s'))) seen $ map (\(_, _, a) -> a) front

eval :: Design -> Design
eval d@Design{memory=Memory{updated=[]}} = d
eval d = eval . step $ d

start :: Design -> Map String Int -> Design
start d@Design {memory=mem, ins, outs} state =
  let inits = Maybe.mapMaybe (swap <.> maybeTyple . first (`Map.lookup` state)) ins
      memory' = L.foldl' (\m (a, n) -> m // (a, extend (bvLength a) $ fromInt n)) mem inits
  in
      d {memory = memory'}

peek :: Design -> Map String (Maybe Int)
peek Design{memory, outs} = Map.fromList $ map (second (toInt . (memory !))) outs

exec :: Design -> Map String Int -> Map String (Maybe Int)
exec d@Design {memory=mem, ins, outs} state =
  let inits = Maybe.mapMaybe (swap <.> maybeTyple . first (`Map.lookup` state)) ins
      memory' = L.foldl' (\m (a, n) -> m // (a, fromInt n)) mem inits
      d' = eval d {memory = memory'}
      memory'' = memory d'
   in Map.fromList $ map (second (toInt . (memory'' !))) outs
