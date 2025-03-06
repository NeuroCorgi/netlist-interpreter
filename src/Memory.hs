{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Memory
  ( Memory (..),
    MemoryLike(..),
    empty,
    clearUpdated,
    BitVector(..),
    BVE(..),
    Bit,
    bit,
    stageUpdates,
    -- mkBitVector,
    -- bitView,
    -- bitVector,
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
import Data.List (foldl1', find, sortBy)
import qualified Data.List as L
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe, mapMaybe, isJust, catMaybes)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Debug.Trace (traceShow, traceShowId)

import Bit
import Data.Function (on)
import Control.Arrow (second)

not :: Bit -> Bit
not = complement

or :: [Bit] -> Bit
or = foldl1' (.|.)

and :: [Bit] -> Bit
and = foldl1' (.&.)

all :: (a -> Bit) -> [a] -> Bit
all f = and . map f

-- zipLongest :: [a] -> [a] -> a -> [(a, a)]
-- zipLongest (a : ra) (b : rb) d = (a, b) : zipLongest ra rb d
-- zipLongest (a : ra) [] d = (a, d) : zipLongest ra [] d
-- zipLongest [] (b : rb) d = (d, b) : zipLongest [] rb d
-- zipLongest [] [] _ = []

toInt :: [Bit] -> Maybe Integer
toInt = foldrM (\d a -> (+) <$> toD d <*> return (a * 2)) 0
  where
    toD :: Bit -> Maybe Integer
    toD H = Just 1
    toD L = Just 0
    toD _ = Nothing

fromInt :: Integer -> [Bit]
fromInt 0 = []
fromInt n
  | n < 0     = neg (fromInt (-n) ++ [L])
  | even n    = L : fromInt n'
  | otherwise = H : fromInt n'
  where n' = n `div` 2

data BVE = W Int | B Bit deriving Show
data BitVector = BitVector [BVE] Bool
  deriving Show

wires :: [BVE] -> [Int]
wires = mapMaybe (\case W w -> Just w; B _ -> Nothing)

signed :: BitVector -> Bool
signed (BitVector _ sign) = sign

class ToBitVector a where
  toBitVector :: a -> BitVector

-- mkBitVector :: [BVE] -> Bool -> BitVector
-- mkBitVector = BitVector

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

bitString :: String -> [Bit]
bitString = reverse . map bit

data Edge = Rising | Falling | Other Bit deriving Show

bvLength :: BitVector -> Int
bvLength (BitVector bv _) = length bv

class MemoryLike c where
  (!!) :: c e -> Int -> e
  update :: [(Int, e)] -> c e -> c e

instance MemoryLike [] where
  (!!) = (L.!!)
  update path mem = go path (zip [0,1..] mem)
    where
      go b@((i, e) : r) l@((j, n) : t)
        | i < j = go r l
        | i == j = e : go r t
        | i > j = n : go b t
      go _ l = map snd l

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

empty :: Int -> Memory []
empty n = Memory
  { mMemory = replicate (n + 1) Z
  , mUpMemory = replicate (n + 1) Z
  , mUpdated = []
  }

clearUpdated :: (MemoryLike a) => Memory a -> Memory a
clearUpdated mem = mem{mUpdated=[]}

(//) :: (MemoryLike a) => Memory a -> (BitVector, [Bit]) -> Memory a
(//) m@(Memory {..}) (BitVector bv _, bits) =
  let patch = sortBy (compare `on` fst) . catMaybes $ zipWith (\case (W i) -> \j -> Just (i, j); (B _) -> const Nothing) bv bits
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

(!) :: MemoryLike a => Memory a -> BitVector -> [Bit]
(!) (Memory {mMemory=mem}) (BitVector bv _) = map (\case B b -> b; W w -> mem !! w) bv

edge :: MemoryLike a => Memory a -> BitVector -> Bool
edge Memory{mUpdated=[]} _  = False
edge Memory{mUpdated=update:_} (BitVector bv _) = isJust $ find ((`elem` wbv) . fst) update
  where wbv = wires bv

extendWith :: Bit -> Int -> [Bit] -> [Bit]
extendWith d n bits =
  let
    rest = length bits - n
  in
    case rest of
      0 -> bits
      r | r > 0 -> take n bits
        | True  -> bits ++ replicate (-r) d

extend :: Int -> [Bit] -> [Bit]
extend = extendWith L

---------------------
-- Unary functions --
---------------------

type UnaryBVFun a = BitVector -> BitVector -> Memory a -> Memory a

unaryFunc :: MemoryLike a => ([Bit] -> [Bit]) -> UnaryBVFun a
unaryFunc f a y mem =
  let to = extend (bvLength y)
      ab = to $ mem ! a
   in mem // (y, to $ f ab)

-- | Logical Not
(!|) :: MemoryLike a => UnaryBVFun a
(!|) = unaryFunc logicalNot

logicalNot :: [Bit] -> [Bit]
logicalNot a = [complement (or a)]

-- | Negation
(-|) :: MemoryLike a => UnaryBVFun a
(-|) = unaryFunc neg

neg :: [Bit] -> [Bit]
neg = inc . bitwiseNot

inc :: [Bit] -> [Bit]
inc = reverse . fst . foldl (\(n, c) a -> (a `xor` c : n, a .&. c)) ([], H)

-- | Bitwise Not
(~|) :: MemoryLike a => UnaryBVFun a
(~|) = unaryFunc bitwiseNot

bitwiseNot :: [Bit] -> [Bit]
bitwiseNot = map not

-- | Positive
(+|) :: MemoryLike a => UnaryBVFun a
(+|) = unaryFunc pos

pos :: [Bit] -> [Bit]
pos a = a

-- | Reduce And
(&/|) :: MemoryLike a => UnaryBVFun a
(&/|) = unaryFunc andReduce

andReduce :: [Bit] -> [Bit]
andReduce a = [and a]

-- | Boolean Reduce
(!!|) :: MemoryLike a => UnaryBVFun a
(!!|) = unaryFunc boolReduce

boolReduce :: [Bit] -> [Bit]
boolReduce = logicalNot . logicalNot

-- | Reduce OR
(|/|) :: MemoryLike a => UnaryBVFun a
(|/|) = unaryFunc orReduce

orReduce :: [Bit] -> [Bit]
orReduce a = [or a]

-- | Reduce XNOR
(!^/|) :: MemoryLike a => UnaryBVFun a
(!^/|) = unaryFunc xnorReduce

xnorReduce :: [Bit] -> [Bit]
xnorReduce a = [all not a .|. and a]

-- | Reduce XOR
(^/|) :: MemoryLike a => UnaryBVFun a
(^/|) = unaryFunc xorReduce

xorReduce :: [Bit] -> [Bit]
xorReduce a = [foldl1 xor a]

------------------------
--  Binary functions  --
------------------------

type BinaryBVFun a = BitVector -> BitVector -> BitVector -> Memory a -> Memory a

binaryFunc :: MemoryLike a => ([Bit] -> [Bit] -> [Bit]) -> BinaryBVFun a
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
    in case last bs of
        H ->  (\n -> n - (2 ^ (bvLength bv - 1))) <$> n
        L -> n
        _ -> Nothing
  | otherwise = toInt $ extend (bvLength bv - 1) (mem ! bv)

binaryFuncMath :: MemoryLike a => (Integer -> Integer -> Maybe Integer) -> BinaryBVFun a
binaryFuncMath f a b y mem
  | signed a && signed b = mem // (y, fromMaybe (replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      res <- an `f` bn
      return $ extendWith (if res < 0 then H else L) (bvLength y) $ fromInt res)
  | otherwise = mem // (y, maybe (replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      res <- an `f` bn
      return $ fromInt res)

math :: (Integer -> Integer -> Maybe Integer) -> Maybe Integer -> Maybe Integer -> [Bit]
math f a b = fromMaybe [X] $ do
  a' <- a
  b' <- b
  fromInt <$> a' `f` b'

-- | Add
(|+|) :: MemoryLike a => BinaryBVFun a
(|+|) = binaryFuncMath (\a b -> Just (a + b))

-- | And
(|&|) :: MemoryLike a => BinaryBVFun a
(|&|) = binaryFunc and'

and' :: [Bit] -> [Bit] -> [Bit]
and'= zipWith (.&.)

-- | Mul

(|*|) :: MemoryLike a => BinaryBVFun a
(|*|) = binaryFuncMath (\a b -> Just (a * b))

-- | Sub

(|-|) :: MemoryLike a => BinaryBVFun a
(|-|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an - bn
      return $ extendWith (if res < 0 then H else L) (bvLength y) $ fromInt res)
  | otherwise = mem // (y, maybe (replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an - bn
      return $ extendWith (if res < 0 then H else L) (bvLength y) $ fromInt res)

-- | Bitwise case equal

(|===.|) :: MemoryLike a => BinaryBVFun a
(|===.|) = binaryFunc bweqx

bweqx :: [Bit] -> [Bit] -> [Bit]
bweqx = zipWith (\a b -> fromBool (a == b))

-- | Div

(|//|) :: MemoryLike a => BinaryBVFun a
(|//|) = binaryFuncMath sDiv
  where
    sDiv _ 0 = Nothing
    sDiv a b = Just (div a b)

-- div' :: [Bit] -> [Bit] -> [Bit]
-- div' = math sDiv

-- | Div Floor

{-# WARNING (|//.|) "Signed bitvectors are not supported, so this functions behaves exactly like div" #-}
(|//.|) :: MemoryLike a => BinaryBVFun a
(|//.|) = binaryFunc floorDiv

floorDiv :: [Bit] -> [Bit] -> [Bit]
floorDiv _a _b = undefined

-- | Mod

modB :: MemoryLike a => BinaryBVFun a
modB = binaryFuncMath sMod
  where
    sMod _ 0 = Nothing
    sMod a b = Just (a `mod` b)

-- mod' :: [Bit] -> [Bit] -> [Bit]
-- mod' = math sMod

-- | Equality

(|==|) :: MemoryLike a => BinaryBVFun a
(|==|) = binaryFunc eq

eq :: [Bit] -> [Bit] -> [Bit]
eq a b = andReduce $ zipWith equal a b

-- | Case equality

(|===|) :: MemoryLike a => BinaryBVFun a
(|===|) = binaryFunc caseEq

{-# WARNING caseEq "Implement case equality" #-}
caseEq :: [Bit] -> [Bit] -> [Bit]
caseEq a b = andReduce $ zipWith (\a' b' -> fromBool (a' == b')) a b

-- | Greater Equal

(|>=|) :: MemoryLike a => BinaryBVFun a
(|>=|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an >= bn
      convertRes res)
  | otherwise = mem // (y, maybe (replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an >= bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res

-- greaterEqual :: [Bit] -> [Bit] -> [Bit]
-- greaterEqual = math

-- | Greater than

(|>|) :: MemoryLike a => BinaryBVFun a
(|>|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an > bn
      convertRes res)
  | otherwise = mem // (y, maybe (replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an > bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res

-- greaterThan :: [Bit] -> [Bit] -> [Bit]
-- greaterThan = math

-- | Less Equal

(|<=|) :: MemoryLike a => BinaryBVFun a
(|<=|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an <= bn
      convertRes res)
  | otherwise = mem // (y, maybe (replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an <= bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res
-- lessEqual :: [Bit] -> [Bit] -> [Bit]
-- lessEqual = math

-- | Less Than

(|<|) :: MemoryLike a => BinaryBVFun a
(|<|) a b y mem
  | signed a && signed b = mem // (y, fromMaybe (replicate (bvLength y) X) $ do
      an <- bvToInt mem a
      bn <- bvToInt mem b
      let res = an < bn
      convertRes res)
  | otherwise = mem // (y, maybe (replicate (bvLength y) X) (extend (bvLength y)) $ do
      an <- toInt (mem ! a)
      bn <- toInt (mem ! b)
      let res = an < bn
      convertRes res)
  where
    convertRes res = Just $ extend (bvLength y) $ fromInt $ fromIntegral $ fromEnum res

-- lessThan :: [Bit] -> [Bit] -> [Bit]
-- lessThan = math

-- | Logic And

(|&&|) :: MemoryLike a => BinaryBVFun a
(|&&|) = binaryFunc lAnd

lAnd :: [Bit] -> [Bit] -> [Bit]
lAnd a b = boolReduce (a `and'` b)

-- | Logic Or

(||||) :: MemoryLike a => BinaryBVFun a
(||||) = binaryFunc lOr

lOr :: [Bit] -> [Bit] -> [Bit]
lOr a b = boolReduce (a `or'` b)

-- | Or

(|||) :: MemoryLike a => BinaryBVFun a
(|||) = binaryFunc or'

or' :: [Bit] -> [Bit] -> [Bit]
or' = zipWith (.|.)

-- | XNor

(|!^|) :: MemoryLike a => BinaryBVFun a
(|!^|) = binaryFunc xnor

xnor :: [Bit] -> [Bit] -> [Bit]
xnor = zipWith ((. not) . xor)

-- | Xor

(|^|) :: MemoryLike a => BinaryBVFun a
(|^|) = binaryFunc xor'

xor' :: [Bit] -> [Bit] -> [Bit]
xor' = zipWith xor

------------------
-- Multeplexers --
------------------

bmux :: MemoryLike a => BitVector -> BitVector -> BitVector -> Memory a -> Memory a
bmux a s _y mem =
  let _ab = mem ! a
      _sb = mem ! s
  in undefined

bwmux :: MemoryLike a => BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
bwmux a b s y mem =
  let ab = mem ! a
      bb = mem ! b
      sb = mem ! s
      res = zipWith3 (\case H -> \a' _ -> a'; L -> \_ b' -> b'; _ -> \_ _ -> X) sb ab bb
  in mem // (y, res)

demux :: MemoryLike a => BitVector -> BitVector -> BitVector -> Memory a -> Memory a
demux a s y mem =
  let ab = mem ! a
      sb = mem ! s
      emptySegment = replicate (length ab) L
      segment d i
        | i == d = ab
        | otherwise = emptySegment
  in case toInt sb of
       Just i -> mem // (y, concatMap (segment i) [0..(2 ^ length sb)])
       Nothing -> mem // (y, replicate (length ab * (2 ^ length sb)) Z)

mux :: MemoryLike a => BitVector -> BitVector -> BitVector -> BitVector -> Memory a -> Memory a
mux a b s y mem =
  let ab = mem ! a
      bb = mem ! b
      [ s' ] = mem ! s
      res = case s' of H -> bb; L -> ab; _ -> replicate (length ab) Z
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
        [ X ] -> mem // (y, replicate (length ab) X)
        _ ->
          case index sb H of
            Just i -> mem // (y, take (bvLength y) $ drop (length ab * i) bb)
            Nothing -> mem // (y, replicate (length ab) X)
  where
    -- Both cases of not finding an element and finding it more than once are resulting in `Nothing`
    index l e = foldl (folder e) Nothing (zip [0,1..] l)
    folder e Nothing (i, a)
      | e == a = Just i
      | otherwise = Nothing
    folder e res (i, a)
      | e == a = Nothing
      | otherwise = res


tribuf :: MemoryLike a => BitVector -> BitVector -> BitVector -> Memory a -> Memory a
tribuf a en y mem =
  let ab = mem ! a
      [ en' ] = mem ! en
  in mem // (y, case en' of H -> ab; L -> replicate (length ab) Z; _ -> replicate (length ab) X)

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
      [ en'  ] = mem ! en
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
  let [ en'  ] = mem ! en
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
      [ en'  ] = mem ! en
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
      [ en'  ] = mem ! en
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
