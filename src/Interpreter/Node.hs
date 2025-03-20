{-# LANGUAGE RecordWildCards #-}

module Interpreter.Node where

import Control.Monad.Fail

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import Data.IntMap (IntMap)
import Data.Foldable (foldlM)

import Interpreter.CompState

import Memory
import Intermediate (Cell(..), Direction(..))
import Intermediate.CellKinds

infixr 9 <.>
(<.>) :: (Functor m) => (b -> c) -> (a -> m b) -> (a -> m c)
(<.>) f g x = f <$> g x

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)


newtype Node = Node (Memory Vector -> Memory Vector, [Int], [Int])

inputs :: Node -> [Int]
inputs (Node (_, ins, _)) = ins

outputs :: Node -> [Int]
outputs (Node (_, _, outs)) = outs

ofCell :: Map String b -> Cell -> DesignStateBuilder (Either String) Node
ofCell subDesignMap Cell {..} =
  case cKind of
    Buf -> do
      width <- num =<< lookup' "WIDTH" cParameters
      a <- validateWidth width =<< lookup' "A" ins
      y <- validateWidth width =<< lookup' "Y" outs
      return (Node (id, wires a, wires y))
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
              (adff (V.head clkPol) (V.head rstPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d, wires q))
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
              (adffe (V.head clkPol) (V.head rstPol) (V.head enPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector en) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires en ++ wires d, wires q))
    Adlatch -> do
      width <- num =<< lookup' "WIDTH" cParameters
      enPol <- bitString <$> lookup' "EN_POLARITY" cParameters
      rstPol<- bitString <$> lookup' "ARST_POLARITY" cParameters
      rstVal <- validateWidth width . bitString =<< lookup' "ARST_VALUE" cParameters
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ARST" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (adlatch (V.head enPol) (V.head rstPol) (toBitVector rstVal) (toBitVector en) (toBitVector rst) (toBitVector d) (toBitVector q), wires en ++ wires rst ++ wires d, wires q))
    Aldff -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol<- bitString <$> lookup' "ALOAD_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (aldff (V.head clkPol) (V.head rstPol) (toBitVector clk) (toBitVector rst) (toBitVector ad) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d ++ wires ad, wires q))
    Aldffe -> do
      width <- num =<< lookup' "WIDTH" cParameters
      enPol <- bitString <$> lookup' "EN_POLARITY" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      rstPol<- bitString <$> lookup' "ALOAD_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      rst <- validateWidth 1 =<< lookup' "ALOAD" ins
      ad <- validateWidth width =<< lookup' "AD" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (aldffe (V.head clkPol) (V.head rstPol) (V.head enPol) (toBitVector clk) (toBitVector rst) (toBitVector en) (toBitVector ad) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires en ++ wires d ++ wires ad, wires q))
    Dff -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (dff (V.head clkPol) (toBitVector clk) (toBitVector d) (toBitVector q), wires clk ++ wires d, wires q))
    Dffe -> do
      width <- num =<< lookup' "WIDTH" cParameters
      clkPol <- bitString <$> lookup' "CLK_POLARITY" cParameters
      enPol <- bitString <$> lookup' "EN_POLARITY" cParameters
      clk <- validateWidth 1 =<< lookup' "CLK" ins
      en <- validateWidth 1 =<< lookup' "EN" ins
      d <- validateWidth width =<< lookup' "D" ins
      q <- validateWidth width =<< lookup' "Q" outs
      return (Node
              (dffe (V.head clkPol) (V.head enPol) (toBitVector clk) (toBitVector en) (toBitVector d) (toBitVector q), wires clk ++ wires en ++ wires d, wires q))
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
              (sdff (V.head clkPol) (V.head rstPol) (toBitVector rstVal) (toBitVector clk) (toBitVector rst) (toBitVector d) (toBitVector q), wires clk ++ wires rst ++ wires d, wires q))
    SubDesign name
      | Just subdesign <- M.lookup name subDesignMap -> do
          ind <- newSubDesign 0
          undefined
      | otherwise -> fail ("Unknown node: " ++ name)
    _ -> fail "Unknown node"
  where
    num :: (MonadFail m) => String -> m Int
    num = foldlM (\y x -> (+) <$> d2i x <*> return (y * 2)) 0
    -- d2i :: (Monad Char -> Either String Int
    d2i '0' = pure 0
    d2i '1' = pure 1
    d2i  _  = fail ""

    validateWidth :: (Foldable t, MonadFail m) => Int -> t a -> m (t a)
    validateWidth n v
      | length v == n = return v
      | otherwise = fail ("inconsistent width: expected " ++ show n ++ ", got " ++ show (length v))

    lookup' :: (Show a, Ord a, MonadFail m) => a -> Map a b -> m b
    lookup' key map =
      case M.lookup key map of
        Just a -> return a
        Nothing -> fail ("Not found key: " ++ show key)

    unaryMath :: MonadFail m => m ([Int], [Int], BitVector, BitVector)
    unaryMath = do
      a_signed <- (== 1) <$> (num =<< lookup' "A_SIGNED" cParameters)
      a_width <- num =<< lookup' "A_WIDTH" cParameters
      y_width <- num =<< lookup' "Y_WIDTH" cParameters
      a <- validateWidth a_width =<< lookup' "A" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      return (wires a, wires y, toBitVector (a, a_signed), toBitVector y)

    binaryMath :: MonadFail m => m ([Int], [Int], [Int], BitVector, BitVector, BitVector)
    binaryMath = do
      a_signed <- (== 1) <$> (num =<< lookup' "A_SIGNED" cParameters)
      b_signed <- (== 1) <$> (num =<< lookup' "B_SIGNED" cParameters)
      a_width <- num =<< lookup' "A_WIDTH" cParameters
      b_width <- num =<< lookup' "B_WIDTH" cParameters
      y_width <- num =<< lookup' "Y_WIDTH" cParameters
      a <- validateWidth a_width =<< lookup' "A" ins
      b <- validateWidth b_width =<< lookup' "B" ins
      y <- validateWidth y_width =<< lookup' "Y" outs
      return (wires a, wires b, wires y, toBitVector (a, a_signed), toBitVector (b, b_signed), toBitVector y)

    (ins, outs) = both M.fromList . L.partition ((fromMaybe False . (== Input) <.> (`M.lookup` cPortDirections)) . fst) $ M.toList cConnections
