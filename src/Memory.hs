{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Memory
  ( Memory (..),
    MemoryLike(..),
    empty,
    clearUpdated,
    BitVector(..),
    ToBitVector(..),
    BVE(..),
    Bit(..),
    bit,
    stageUpdates,
    bitString,
    extend,
    bvLength,
    wires,
    toBitVector,
    bvToInt,
    (//),
    (!),
    (!|),
    (-|),
    (~|),
    (+|),
    (&/|),
    (!!|),
    (|/|),
    (!^/|),
    (^/|)

    , (|+|)
    , (|&|)
    , (|*|)
    , (|-|)
    , (|==|)
    , (|===|)
    , (|===.|)
    , (|//|)
    , (|//.|)
    , (|||)
    , (|!^|)
    , (|^|)
    , (|&&|)
    , (||||)
    , (|<=|)
    , (|<|)
    , (|>=|)
    , (|>|)
    , modB

    , bmux
    , bwmux
    , demux
    , mux
    , pmux
    , tribuf

    , adff
    , adffe
    , adlatch
    , aldff
    , aldffe
    , dff
    , dffe
    , sdff

    , toInt
    , fromInt
  )
where

import Prelude hiding (not, or, and, all, (!!))

import Control.Arrow (second)

import Data.List (find, sortBy)
-- import qualified Data.List as L

import Data.Data (Data)

import Data.Vector (Vector)
import qualified Data.Vector as V
-- import qualified Data.Array.Utils as AU

import Data.Maybe (fromMaybe, mapMaybe, isJust, catMaybes)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Memory.Bit

import Data.Function (on)
import Data.Foldable (foldrM, find)

import GHC.Base hiding (not, empty)
import GHC.Integer.Logarithms

not :: Bit -> Bit
not = complement

type BitA = Vector Bit

or :: BitA -> Bit
or = foldl1 (.|.)

and :: BitA -> Bit
and = foldl1 (.&.)

all :: (a -> Bit) -> Vector a -> Bit
all f = and . fmap f

toInt :: BitA -> Maybe Integer
toInt = foldrM (\d a -> (+) <$> toD d <*> return (a * 2)) 0
  where
    toD :: Bit -> Maybe Integer
    toD H = Just 1
    toD L = Just 0
    toD _ = Nothing

fromInt :: Integer -> BitA
fromInt n
  | n < 0 = neg $ V.fromList (bits (-n) ++ [L])
  | otherwise = V.fromList (bits n)
  where
    -- bits n =
    bits 0 = []
    bits n
      | even n    = L : bits n'
      | otherwise = H : bits n'
      where n' = n `div` 2

clog2 :: Integer -> Int
clog2 x = fromMaybe 0 $ clogBase 2 x

clogBase :: Integer -> Integer -> Maybe Int
clogBase x y | x > 1 && y > 0 =
  case y of
    1 -> Just 0
    _ -> let z1 = integerLogBase# x y
             z2 = integerLogBase# x (y-1)
         in  if isTrue# (z1 ==# z2)
                then Just (I# (z1 +# 1#))
                else Just (I# z1)
clogBase _ _ = Nothing

data BVE = W Int | B Bit deriving (Show, Data)

data BitVector = BitVector [BVE] Bool
  deriving Show

wires :: [BVE] -> [Int]
wires = mapMaybe (\case W w -> Just w; B _ -> Nothing)

signed :: BitVector -> Bool
signed (BitVector _ sign) = sign

class ToBitVector a where
  toBitVector :: a -> BitVector

instance ToBitVector ([BVE], Bool) where
  toBitVector = uncurry BitVector

instance ToBitVector [BVE] where
  toBitVector bve = BitVector bve False

instance ToBitVector ([Int], Bool) where
  toBitVector (p, sign) = BitVector (map W p) sign

instance ToBitVector [Int] where
  toBitVector p = BitVector (map W p) False

instance ToBitVector [Bit] where
  toBitVector p = BitVector (map B p) False

instance ToBitVector (Vector Bit) where
  toBitVector p = BitVector (map B $ V.toList p) False

-- bitView :: [Int] -> BitVector
-- bitView p = BitVector (map W p) False
--
-- bitVector :: [Bit] -> BitVector
-- bitVector b = BitVector (map B b) False

bit :: Char -> Bit
bit '1' = H
bit '0' = L
bit 'z' = Z
bit 'x' = X
bit  _  = undefined

bitString :: String -> BitA
bitString s = V.fromList bits
  where bits = reverse $ map bit s

data Edge = Rising | Falling | Other Bit deriving Show

bvLength :: BitVector -> Int
bvLength (BitVector bv _) = length bv

class MemoryLike c where
  (!!) :: c e -> Int -> e
  update :: [(Int, e)] -> c e -> c e

instance MemoryLike Vector where
  (!!) = (V.!)
  update patch mem = mem V.// patch

instance MemoryLike IntMap where
  (!!) = (IM.!)
  update patch mem = IM.fromList patch `IM.union` mem

data (MemoryLike a) => Memory a =
  Memory
  { mMemory :: a Bit
  , mUpMemory :: a Bit
  , mUpdated :: [[(Int, Edge)]]
  }

stageUpdates :: (MemoryLike a) => Memory a -> Memory a
stageUpdates unstaged@Memory{..} = unstaged{mMemory = newMemory, mUpMemory = newMemory}
  where
    newMemory = update patch mMemory

    patch = sortBy (compare `on` fst) $ concatMap (map (second fromEdge)) mUpdated

    fromEdge Rising = H
    fromEdge Falling = L
    fromEdge (Other b) = b

deriving instance (MemoryLike a, Show (a Bit)) => Show (Memory a)

empty :: Int -> Memory Vector
empty n = Memory
  { mMemory = V.replicate (n + 1) Z
  , mUpMemory = V.replicate (n + 1) Z
  , mUpdated = []
  }

clearUpdated :: (MemoryLike a) => Memory a -> Memory a
clearUpdated mem = mem{mUpdated=[]}

(//) :: (MemoryLike a) => Memory a -> (BitVector, BitA) -> Memory a
(//) m@(Memory {..}) (BitVector bv _, bits) =
  let patch = sortBy (compare `on` fst) . catMaybes $ zipWith (\case (W i) -> \j -> Just (i, j); (B _) -> const Nothing) bv (V.toList bits)
      memory' = update patch mUpMemory
      changed = mapMaybe (\i -> (i ,) <$> maybeEdge (mMemory !! i) (memory' !! i)) $ wires bv
  in m{ mUpMemory = memory'
      , mUpdated = mUpdated ++ [changed]
      }
  where
    maybeEdge :: Bit -> Bit -> Maybe Edge
    maybeEdge _ H = Just Rising
    maybeEdge _ L = Just Falling
    maybeEdge a b
      | a /= b = Just (Other b)
      | otherwise = Nothing

(!) :: MemoryLike a => Memory a -> BitVector -> BitA
(!) (Memory {mMemory=mem}) (BitVector bv _) = V.fromList $ map (\case B b -> b; W w -> mem !! w) bv

edge :: MemoryLike a => Memory a -> BitVector -> Bool
edge Memory{mUpdated=[]} _  = False
edge Memory{mUpdated=update:_} (BitVector bv _) = isJust $ find ((`elem` wbv) . fst) update
  where wbv = wires bv

extendWith :: Bit -> Int -> BitA -> BitA
extendWith d n bits =
  let
    rest = V.length bits - n
  in
    case rest of
      0 -> bits
      r | r >= 0     -> V.take n bits
        | otherwise -> bits V.++ V.replicate (-r) d

extend :: Int -> BitA -> BitA
extend = extendWith L

sB :: Bit -> BitA
sB = V.singleton

---------------------
-- Unary functions --
---------------------

type UnaryBVFun a = BitVector -> BitVector -> Memory a -> Memory a

unaryFunc :: MemoryLike a => (BitA -> BitA) -> UnaryBVFun a
unaryFunc f a y mem =
  let to = extend (bvLength y)
      ab = mem ! a
   in mem // (y, to $ f ab)

-- | Logical Not
(!|) :: MemoryLike a => UnaryBVFun a
(!|) = unaryFunc logicalNot

logicalNot :: BitA -> BitA
logicalNot a = sB $ complement (or a)

-- | Negation
(-|) :: MemoryLike a => UnaryBVFun a
(-|) = unaryFunc neg

neg :: BitA -> BitA
neg = inc . bitwiseNot

inc :: BitA -> BitA
-- inc = V.foldMap (\c a -> (a .&. c, a `xor` c)) H
inc = V.fromList . reverse . fst . foldl (\(n, c) a -> (a `xor` c : n, a .&. c)) ([], H)

-- | Bitwise Not
(~|) :: MemoryLike a => UnaryBVFun a
(~|) = unaryFunc bitwiseNot

bitwiseNot :: BitA -> BitA
bitwiseNot = fmap not

-- | Positive
(+|) :: MemoryLike a => UnaryBVFun a
(+|) = unaryFunc pos

pos :: BitA -> BitA
pos a = a

-- | Reduce And
(&/|) :: MemoryLike a => UnaryBVFun a
(&/|) = unaryFunc andReduce

andReduce :: BitA -> BitA
andReduce a = sB $ and a

-- | Boolean Reduce
(!!|) :: MemoryLike a => UnaryBVFun a
(!!|) = unaryFunc boolReduce

boolReduce :: BitA -> BitA
boolReduce = logicalNot . logicalNot

-- | Reduce OR
(|/|) :: MemoryLike a => UnaryBVFun a
(|/|) = unaryFunc orReduce

orReduce :: BitA -> BitA
orReduce a = sB $ or a

-- | Reduce XNOR
(!^/|) :: MemoryLike a => UnaryBVFun a
(!^/|) = unaryFunc xnorReduce

xnorReduce :: BitA -> BitA
xnorReduce a = sB (all not a .|. and a)

-- | Reduce XOR
(^/|) :: MemoryLike a => UnaryBVFun a
(^/|) = unaryFunc xorReduce

xorReduce :: BitA -> BitA
xorReduce a = sB $ foldl1 xor a

------------------------
--  Binary functions  --
------------------------

type BinaryBVFun a = BitVector -> BitVector -> BitVector -> Memory a -> Memory a

binaryFunc :: MemoryLike a => (BitA -> BitA -> BitA) -> BinaryBVFun a
binaryFunc f a b y mem =
  let toA = extend (max (bvLength a) (bvLength b))
      toR = extend (bvLength y)
      ab = toA $ mem ! a
      bb = toA $ mem ! b
  in mem // (y, toR $ ab `f` bb)

bvToInt :: MemoryLike a => Memory a -> BitVector -> Maybe Integer
bvToInt mem bv@(BitVector _ sign)
  | sign =
    let bs = mem ! bv
        n = toInt $ extend (bvLength bv - 1) bs
    in case V.last bs of
        H ->  (\n -> n - (2 ^ (bvLength bv - 1))) <$> n
        L -> n
        _ -> Nothing
  | otherwise = toInt $ extend (bvLength bv - 1) (mem ! bv)

binaryFuncMath :: MemoryLike a => (Integer -> Integer -> Maybe Integer) -> BinaryBVFun a
binaryFuncMath f a b y mem
  | signed a && signed b = mem // (y, fromMaybe (V.replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      res <- an `f` bn
      return $ extendWith (if res < 0 then H else L) (bvLength y) $ fromInt res)
  | otherwise = mem // (y, maybe (V.replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      res <- an `f` bn
      return $ fromInt res)

-- math :: (Integer -> Integer -> Maybe Integer) -> Maybe Integer -> Maybe Integer -> BitA
-- math f a b = fromMaybe [X] $ do
--   a' <- a
--   b' <- b
--   fromInt <$> a' `f` b'

-- | Add
(|+|) :: MemoryLike a => BinaryBVFun a
(|+|) = binaryFuncMath (\a b -> Just (a + b))

-- | And
(|&|) :: MemoryLike a => BinaryBVFun a
(|&|) = binaryFunc and'

and' :: BitA -> BitA -> BitA
and'= V.zipWith (.&.)

-- | Mul

(|*|) :: MemoryLike a => BinaryBVFun a
(|*|) = binaryFuncMath (\a b -> Just (a * b))

-- | Sub

(|-|) :: MemoryLike a => BinaryBVFun a
(|-|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (V.replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an - bn
      return $ extendWith (if res < 0 then H else L) (bvLength y) $ fromInt res)
  | otherwise = mem // (y, maybe (V.replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an - bn
      return $ extendWith (if res < 0 then H else L) (bvLength y) $ fromInt res)

-- | Bitwise case equal

(|===.|) :: MemoryLike a => BinaryBVFun a
(|===.|) = binaryFunc bweqx

bweqx :: BitA -> BitA -> BitA
bweqx = V.zipWith (\a b -> fromBool (a == b))

-- | Div

(|//|) :: MemoryLike a => BinaryBVFun a
(|//|) = binaryFuncMath sDiv
  where
    sDiv _ 0 = Nothing
    sDiv a b = Just (div a b)

-- div' :: BitA -> BitA -> BitA
-- div' = math sDiv

-- | Div Floor

{-# WARNING (|//.|) "Signed bitvectors are not supported, so this functions behaves exactly like div" #-}
(|//.|) :: MemoryLike a => BinaryBVFun a
(|//.|) = binaryFunc floorDiv

floorDiv :: BitA -> BitA -> BitA
floorDiv _a _b = undefined

-- | Mod

modB :: MemoryLike a => BinaryBVFun a
modB = binaryFuncMath sMod
  where
    sMod _ 0 = Nothing
    sMod a b = Just (a `mod` b)

-- mod' :: BitA -> BitA -> BitA
-- mod' = math sMod

-- | Equality

(|==|) :: MemoryLike a => BinaryBVFun a
(|==|) = binaryFunc eq

eq :: BitA -> BitA -> BitA
eq a b = andReduce $ V.zipWith equal a b

-- | Case equality

(|===|) :: MemoryLike a => BinaryBVFun a
(|===|) = binaryFunc caseEq

{-# WARNING caseEq "Implement case equality" #-}
caseEq :: BitA -> BitA -> BitA
caseEq a b = andReduce $ V.zipWith (\a' b' -> fromBool (a' == b')) a b

-- | Greater Equal

(|>=|) :: MemoryLike a => BinaryBVFun a
(|>=|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (V.replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an >= bn
      convertRes res)
  | otherwise = mem // (y, maybe (V.replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an >= bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res

-- greaterEqual :: BitA -> BitA -> BitA
-- greaterEqual = math

-- | Greater than

(|>|) :: MemoryLike a => BinaryBVFun a
(|>|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (V.replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an > bn
      convertRes res)
  | otherwise = mem // (y, maybe (V.replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an > bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res

-- greaterThan :: BitA -> BitA -> BitA
-- greaterThan = math

-- | Less Equal

(|<=|) :: MemoryLike a => BinaryBVFun a
(|<=|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (V.replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an <= bn
      convertRes res)
  | otherwise = mem // (y, maybe (V.replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an <= bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res
-- lessEqual :: BitA -> BitA -> BitA
-- lessEqual = math

-- | Less Than

(|<|) :: MemoryLike a => BinaryBVFun a
(|<|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (V.replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an < bn
      convertRes res)
  | otherwise = mem // (y, maybe (V.replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an < bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res

-- lessThan :: BitA -> BitA -> BitA
-- lessThan = math

-- | Logic And

(|&&|) :: MemoryLike a => BinaryBVFun a
(|&&|) = binaryFunc lAnd

lAnd :: BitA -> BitA -> BitA
lAnd a b = boolReduce (a `and'` b)

-- | Logic Or

(||||) :: MemoryLike a => BinaryBVFun a
(||||) = binaryFunc lOr

lOr :: BitA -> BitA -> BitA
lOr a b = boolReduce (a `or'` b)

-- | Or

(|||) :: MemoryLike a => BinaryBVFun a
(|||) = binaryFunc or'

or' :: BitA -> BitA -> BitA
or' = V.zipWith (.|.)

-- | XNor

(|!^|) :: MemoryLike a => BinaryBVFun a
(|!^|) = binaryFunc xnor

xnor :: BitA -> BitA -> BitA
xnor = V.zipWith ((. not) . xor)

-- | Xor

(|^|) :: MemoryLike a => BinaryBVFun a
(|^|) = binaryFunc xor'

xor' :: BitA -> BitA -> BitA
xor' = V.zipWith xor

------------------
-- Multeplexers --
------------------

bmux :: MemoryLike a => BitVector -> BitVector -> BitVector -> Memory a -> Memory a
bmux a s y mem =
  let ab = mem ! a
      sb = mem ! s
  in mem // (y, case toInt sb of
                Just i -> V.slice (fromInteger i * width) width ab
                Nothing -> V.replicate width X)
  where width = bvLength y

bwmux :: MemoryLike a => BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
bwmux a b s y mem =
  let ab = mem ! a
      bb = mem ! b
      sb = mem ! s
      res = V.zipWith3 (\case H -> \a' _ -> a'; L -> \_ b' -> b'; _ -> \_ _ -> X) sb ab bb
  in mem // (y, res)

demux :: MemoryLike a => BitVector -> BitVector -> BitVector -> Memory a -> Memory a
demux a s y mem =
  let ab = mem ! a
      sb = mem ! s
      emptySegment = V.replicate (length ab) L
      segment d i
        | i == d = ab
        | otherwise = emptySegment
  in case toInt sb of
       Just i -> mem // (y, V.concat [segment i s | s <- [0..(2 ^ length sb)]])
       Nothing -> mem // (y, V.replicate (length ab * (2 ^ length sb)) Z)

mux :: MemoryLike a => BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
mux a b s y mem =
  let ab = mem ! a
      bb = mem ! b
      [ s' ] = mem ! s
      res = case s' of H -> bb; L -> ab; _ -> V.replicate (length ab) Z
  in mem // (y, res)

pmux :: MemoryLike a => BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
pmux a b s y mem =
  let ab = mem ! a
      bb = mem ! b
      sb = mem ! s
   in case orReduce sb of
        -- checks if there is no high bits
        [ L ] -> mem // (y, ab)
        -- checks if there are undefined bits
        [ X ] -> mem // (y, V.replicate (length ab) X)
        _ ->
          case index sb H of
            Just i -> mem // (y, V.slice (length ab * i) (bvLength y) bb)
            Nothing -> mem // (y, V.replicate (length ab) X)
  where
    -- Both cases of not finding an element and finding it more than once are resulting in `Nothing`
    index l e =
      let inds = V.elemIndices e l in
        if V.length inds == 1 then Just (inds V.! 0) else Nothing


tribuf :: MemoryLike a => BitVector -> BitVector -> BitVector -> Memory a -> Memory a
tribuf a en y mem =
  let ab = mem ! a
      [ en' ] = mem ! en
  in mem // (y, case en' of H -> ab; L -> V.replicate (length ab) Z; _ -> V.replicate (length ab) X)

---------------
-- Registers --
---------------

adff :: MemoryLike a => Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
adff clkPol rstPol rstVal clk rst d q mem =
  let [ clk' ] = mem ! clk
      [ rst' ] = mem ! rst
      rstVal' = mem ! rstVal
   in
    if edge mem rst || edge mem clk then
      if rst' `equal'` rstPol then
        mem // (q, rstVal')
      else if clk' `equal'` clkPol then
        mem // (q, mem ! d)
      else
        mem
    else mem

adffe :: MemoryLike a => Bit -> Bit -> Bit -> BitVector
      -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector
      -> Memory a -> Memory a
adffe clkPol rstPol enPol rstVal clk rst en d q mem =
  let [ clk' ] = mem ! clk
      [ rst' ] = mem ! rst
      [ en' ]  = mem ! en
      rstVal' = mem ! rstVal
   in if edge mem rst || edge mem clk then
    if rst' `equal'` rstPol then
      mem // (q, rstVal')
    else if clk' `equal'` clkPol && en' `equal'` enPol then
      mem // (q, mem ! d)
    else
      mem
  else
    mem

adlatch :: MemoryLike a => Bit -> Bit -> BitVector
        -> BitVector -> BitVector -> BitVector -> BitVector
        -> Memory a -> Memory a
adlatch enPol rstPol rstVal en rst d q mem =
  let [ en' ]  = mem ! en
      [ rst' ] = mem ! rst
      rstVal' = mem ! rstVal
   in if rst' `equal'` rstPol then
    mem // (q, rstVal')
  else if en' `equal'` enPol then
    mem // (q, mem ! d)
  else
    mem

aldff :: MemoryLike a => Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
aldff clkPol rstPol clk rst ad d q mem =
  let [ clk' ] = mem ! clk
      [ rst' ] = mem ! rst
   in
    if edge mem rst || edge mem clk then
      if rst' `equal'` rstPol then
        mem // (q, mem ! ad)
      else if clk' `equal'` clkPol then
        mem // (q, mem ! d)
      else
        mem
    else mem

aldffe :: MemoryLike a => Bit -> Bit -> Bit -> BitVector
      -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector
      -> Memory a -> Memory a
aldffe clkPol rstPol enPol clk rst en ad d q mem =
  let [ clk' ] = mem ! clk
      [ rst' ] = mem ! rst
      [ en' ]  = mem ! en
   in if edge mem rst || edge mem clk then
    if rst' `equal'` rstPol then
      mem // (q, mem ! ad)
    else if clk' `equal'` clkPol && en' `equal'` enPol then
      mem // (q, mem ! d)
    else
      mem
  else
    mem

dff :: MemoryLike a => Bit -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
dff clkPol clk d q mem =
  let [ clk' ] = mem ! clk
   in if clk' `equal'` clkPol && edge mem clk then
    mem // (q, mem ! d)
  else
    mem

dffe :: MemoryLike a => Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
dffe clkPol enPol clk en d q mem =
  let [ clk' ] = mem ! clk
      [ en' ]  = mem ! en
  in if (clk' `equal'` clkPol && edge mem clk) && (en' `equal'` enPol) then
    mem // (q, mem ! d)
  else
    mem

-- dffsr :: Bit -> Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector -> Memory -> Memory
-- dffsr clkPol setPol rstPol clk set rst d q mem =
--   let [ clk' ] = mem ! clk
--       set' = mem ! set
--       rst' = mem ! rst
--    in

sdff :: MemoryLike a => Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
sdff clkPol rstPol rstVal clk rst d q mem =
  let [ clk' ] = mem ! clk
      [ rst' ] = mem ! rst
   in if clk' `equal'` clkPol && edge mem clk then
    if rst' `equal'` rstPol then
      mem // (q, mem ! rstVal)
    else
      mem // (q, mem ! d)
  else
    mem
