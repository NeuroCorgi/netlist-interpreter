{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Graph where

import Control.Arrow (first, second, (&&&), (***))
import Data.Foldable (foldlM)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)

import Debug.Trace (trace, traceShowId)

import MyLib

import Memory hiding (memory)
import Data.List (nub, intercalate)

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

type Node = (Memory -> Memory, [Int])

inputs :: Node -> [Int]
inputs (_, ins) = ins

ofCell :: Cell -> Either String Node
ofCell Cell {..} =
  case typ of
    "$buf" -> do
      width <- num =<< lookup' "WIDTH" parameters
      a <- validateWidth width =<< lookup' "A" ins
      y <- validateWidth width =<< lookup' "Y" outs
      Right (id, wires a)
    "$logic_not" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((!|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$neg" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((-|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$not" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((~|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$pos" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((+|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_and" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((&/|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_bool" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((!!|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_or" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((^/|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_xnor" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((!^/|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$reduce_xor" -> do
      (a_signed, a, y, ab, yb) <- unaryMath
      case a_signed of
        -- Unsigned
        0 -> Right ((^/|) ab yb, a)
        -- Signed
        _ -> Left "unsupported"
    "$add" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|+|) ab bb yb, a ++ b)
    "$and" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|&|) ab bb yb, a ++ b)
    "$bweqx" -> do
      width <- num =<< lookup' "WIDTH" parameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      y <- validateWidth width =<< lookup' "Y" outs
      Right ((|===.|) (BitVector a) (BitVector b) (BitVector y), wires a ++ wires b)
    "$div" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|//|) ab bb yb, a ++ b)
    "$divfloor" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported" -- Do some other division here
        _ -> Right ((|//|) ab bb yb, a ++ b)
    "$mod" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right (modB ab bb yb, a ++ b)
    "$modfloor" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported" -- Do some other modision here
        _ -> Right (modB ab bb yb, a ++ b)
    "$eq" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|==|) ab bb yb, a ++ b)
    "$eqx" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|===|) ab bb yb, a ++ b)
    "$ge" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|>=|) ab bb yb, a ++ b)
    "$gt" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|<|) ab bb yb, a ++ b)
    "$le" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|<=|) ab bb yb, a ++ b)
    "$lt" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|<|) ab bb yb, a ++ b)
    "$logical_and" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((|&&|) ab bb yb, a ++ b)
    "$logical_or" -> do
      (a_signed, b_signed, a, b, y, ab, bb, yb) <- binaryMath
      case (a_signed, b_signed) of
        (1, 1) -> Left "unsupported"
        _ -> Right ((||||) ab bb yb, a ++ b)
    "$bmux" -> do
      width <- num =<< lookup' "WIDTH" parameters
      s_width <- num =<< lookup' "S_WIDTH" parameters
      a <- validateWidth (2 ^ s_width * width) =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (bmux (BitVector a) (BitVector s) (BitVector y), wires a ++ wires s)
    "$bwmux" -> do
      width <- num =<< lookup' "WIDTH" parameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth width =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (bwmux (BitVector a) (BitVector b) (BitVector s) (BitVector y), wires a ++ wires b ++ wires s)
    "$demux" -> do
      width <- num =<< lookup' "WIDTH" parameters
      s_width <- num =<< lookup' "S_WIDTH" parameters
      a <- validateWidth width =<< lookup' "A" ins
      s <- validateWidth s_width =<< lookup' "S" ins
      y <- validateWidth (width * 2 ^ s_width) =<< lookup' "Y" outs
      return
        (demux (BitVector a) (BitVector s) (BitVector y), wires a ++ wires s)
    "$mux" -> do
      width <- num =<< lookup' "WIDTH" parameters
      a <- validateWidth width =<< lookup' "A" ins
      b <- validateWidth width =<< lookup' "B" ins
      s <- validateWidth 1 =<< lookup' "S" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (mux (BitVector a) (BitVector b) (BitVector s) (BitVector y), wires a ++ wires b ++ wires s)
    "$tribuf" -> do
      width <- num =<< lookup' "WIDTH" parameters
      a <- validateWidth width =<< lookup' "A" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return
        (tribuf (BitVector a) (BitVector en) (BitVector y), wires a ++ wires en)
    "$adff" -> do
      width <- num =<< lookup' "WIDTH" parameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" parameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ARST_POLARITY" parameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" parameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (adff (head clkPol) (head rstPol) (bitVector rstVal) (BitVector clk) (BitVector rst) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires d)
    "$adffe" -> do
      width <- num =<< lookup' "WIDTH" parameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" parameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" parameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ARST_POLARITY" parameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" parameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (adffe (head clkPol) (head rstPol) (head enPol) (bitVector rstVal) (BitVector clk) (BitVector rst) (BitVector en) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires en ++ wires d)
    "$adlatch" -> do
      width <- num =<< lookup' "WIDTH" parameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" parameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ARST_POLARITY" parameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" parameters
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (adlatch (head enPol) (head rstPol) (bitVector rstVal) (BitVector en) (BitVector rst) (BitVector d) (BitVector q), wires en ++ wires rst ++ wires d)
    "$aldff" -> do
      width <- num =<< lookup' "WIDTH" parameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" parameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ALOAD_POLARITY" parameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (aldff (head clkPol) (head rstPol) (BitVector clk) (BitVector rst) (BitVector ad) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires d ++ wires ad)
    "$aldffe" -> do
      width <- num =<< lookup' "WIDTH" parameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" parameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" parameters
      rstPol<- (validateWidth 1 . bitString) =<< lookup' "ALOAD_POLARITY" parameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (aldffe (head clkPol) (head rstPol) (head enPol) (BitVector clk) (BitVector rst) (BitVector en) (BitVector ad) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires en ++ wires d ++ wires ad)
    "$dff" -> do
      width <- num =<< lookup' "WIDTH" parameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" parameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (dff (head clkPol) (BitVector clk) (BitVector d) (BitVector q), wires clk ++ wires d)
    "$dffe" -> do
      width <- num =<< lookup' "WIDTH" parameters
      clkPol <- (validateWidth 1 . bitString) =<< lookup' "CLK_POLARITY" parameters
      enPol <- (validateWidth 1 . bitString) =<< lookup' "EN_POLARITY" parameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (dffe (head clkPol) (head enPol) (BitVector clk) (BitVector en) (BitVector d) (BitVector q), wires clk ++ wires en ++ wires d)
    "$sdff" -> do
      width <- num =<< lookup' "WIDTH" parameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" parameters
      rstPol <- bitString <$> lookup' "SRST_POLARITY" parameters
      rstVal <- validateWidth width . bitString =<< lookup' "SRST_VALUE" parameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "SRST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return
        (sdff (head clkPol) (head rstPol) (bitVector rstVal) (BitVector clk) (BitVector rst) (BitVector d) (BitVector q), wires clk ++ wires rst ++ wires d)
    other -> Left ("Unknown node: " ++ other)
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
      a_signed <- num =<< lookup' "A_SIGNED" parameters
      a_width <- num =<< lookup' "A_WIDTH" parameters
      y_width <- num =<< lookup' "Y_WIDTH" parameters
      a <- validateWidth a_width =<< lookup' "A" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      Right (a_signed, wires a, wires y, BitVector a, BitVector y)

    binaryMath :: Either String (Int, Int, [Int], [Int], [Int], BitVector, BitVector, BitVector)
    binaryMath = do
      a_signed <- num =<< lookup' "A_SIGNED" parameters
      b_signed <- num =<< lookup' "B_SIGNED" parameters
      a_width <- num =<< lookup' "A_WIDTH" parameters
      b_width <- num =<< lookup' "B_WIDTH" parameters
      y_width <- num =<< lookup' "Y_WIDTH" parameters
      a <- validateWidth a_width =<< lookup' "A" ins
      b <- validateWidth b_width =<< lookup' "B" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      Right (a_signed, b_signed, wires a, wires b, wires y, BitVector a, BitVector b, BitVector y)

    (ins, outs) = both Map.fromList . List.partition ((fromMaybe False . (== Input) <.> (`Map.lookup` port_directions)) . fst) $ Map.toList connections

data Design = Design
  { nodes :: [Node]
  , ins :: [(String, BitVector)]
  , outs :: [(String, BitVector)]
  , memory :: Memory
  , update_map :: Map Int Int
  }

instance Show Design where
  show Design{..} =
    "Design{ins=[" ++ show ins ++ "], outs=[" ++ show outs ++ "], update_map=" ++ show update_map ++ ", memory=" ++ show memory ++ "}"

compile :: Module -> Either String Design
compile Module {..} = do
  nodes <- mapM ofCell cells
  let updateMap = Map.fromList . concatMap (\(i, (_, nodeIns)) -> map (, i) nodeIns) $ zip [0..] nodes
  return Design
    { nodes = nodes
    , ins = ins
    , outs = outs
    , memory = empty n_bits
    , update_map = updateMap
    }
  where
    n_bits = uncurry max . (pb *** nb) $ (ports, netnames)
    pb = maximum . map (maximum . pBits)
    nb = maximum . map (maximum . nBits)

    ins = map (pName &&& (bitView . pBits)) $ filter ((== Input) . direction) ports
    outs = map (pName &&& (bitView . pBits)) $ filter ((== Output) . direction) ports

step :: Design -> Design
step d@Design {memory, nodes, update_map} = d{memory=go memory}
  where
    go mem@Memory{updated=[]} = mem
    go oldMem@Memory{updated=update:_} = newMem
    -- The first update block should persist to allow to detect *all* edges
      where
        -- runningMem = oldMem{updated=ups}
        influencedNodesInds = traceShowId (nub $ mapMaybe ((`Map.lookup` update_map) . fst) update)
        influencedNodes = map (fst . (nodes !!)) influencedNodesInds
        endMem@Memory{updated=_:rest} = foldl (\mem node -> node mem) oldMem influencedNodes
        newMem = endMem{updated=rest}

eval :: Design -> Design
eval d@Design{memory=Memory{updated=[]}} = d
eval d = eval . step $ d

start :: Design -> Map String Int -> Design
start d@Design {memory=mem, ins, outs} state =
  let inits = Maybe.mapMaybe (swap <.> maybeTyple . first (`Map.lookup` state)) ins
      memory' = List.foldl' (\m (a, n) -> m // (a, extend (bvLength a) $ fromInt n)) mem inits
  in
      d {memory = memory'}

peek :: Design -> Map String (Maybe Int)
peek Design{memory, outs} = Map.fromList $ map (second (toInt . (memory !))) outs

exec :: Design -> Map String Int -> Map String (Maybe Int)
exec d@Design {memory=mem, ins, outs} state =
  let inits = Maybe.mapMaybe (swap <.> maybeTyple . first (`Map.lookup` state)) ins
      memory' = List.foldl' (\m (a, n) -> m // (a, fromInt n)) mem inits
      d' = eval d {memory = memory'}
      memory'' = memory d'
   in Map.fromList $ map (second (toInt . (memory'' !))) outs
