{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Memory
  ( Memory (..),
    empty,
    clearUpdated,
    BitVector(..),
    BVE(..),
    Bit,
    bit,
    bitView,
    bitVector,
    bitString,
    extend,
    bvLength,
    wires,
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

import Prelude hiding (not, or, and, all)
import Data.List (foldl1', find)
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe, mapMaybe, isJust, catMaybes)

import Debug.Trace (traceShow, traceShowId)

import Bit

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

toInt :: [Bit] -> Maybe Int
toInt = foldrM (\d a -> (+) <$> toD d <*> return (a * 2)) 0
  where
    toD :: Bit -> Maybe Int
    toD H = Just 1
    toD L = Just 0
    toD _ = Nothing

fromInt :: Int -> [Bit]
fromInt 0 = []
fromInt n
  | even n    = L : fromInt n'
  | otherwise = H : fromInt n'
  where n' = n `div` 2

data BVE = W Int | B Bit deriving Show
newtype BitVector = BitVector [BVE]
  deriving Show

wires :: [BVE] -> [Int]
wires = mapMaybe (\case W w -> Just w; B _ -> Nothing)

bitView :: [Int] -> BitVector
bitView = BitVector . map W

bitVector :: [Bit] -> BitVector
bitVector = BitVector . map B

bit :: Char -> Bit
bit '1' = H
bit '0' = L
bit 'z' = Z
bit 'x' = X
bit  _  = undefined

bitString :: String -> [Bit]
bitString = reverse . map bit

data Edge = Rising | Falling deriving Show

bvLength :: BitVector -> Int
bvLength (BitVector bv) = length bv

data Memory =
  Memory
  { memory :: [Bit]
  , updated :: [[(Int, Edge)]]
  } deriving Show

empty :: Int -> Memory
empty n = Memory
  { memory = replicate (n + 1) Z
  , updated = []
  }

clearUpdated :: Memory -> Memory
clearUpdated mem = mem{updated=[]}

(//) :: Memory -> (BitVector, [Bit]) -> Memory
(//) m@(Memory {..}) (BitVector bv, bits) =
  let patch = catMaybes $ zipWith (\case (W i) -> \j -> Just (i, j); (B _) -> const Nothing) bv bits
      memory' = go patch (zip [0,1..] memory)
      changed = mapMaybe (\i -> (i ,) <$> maybeEdge (memory !! i) (memory' !! i)) $ wires bv
  in m{ memory = memory'
      , updated = updated ++ [changed]
      }
  where
    go b@((i, e) : r) l@((j, n) : t)
      | i < j = go r l
      | i == j = e : go r t
      | i > j = n : go b t
    go _ l = map snd l

    maybeEdge :: Bit -> Bit -> Maybe Edge
    maybeEdge H H = Nothing
    maybeEdge _ H = Just Rising
    maybeEdge L L = Nothing
    maybeEdge _ L = Just Falling
    maybeEdge _ _ = Nothing

(!) :: Memory -> BitVector -> [Bit]
(!) (Memory {memory=mem}) (BitVector bv) = map (\case B b -> b; W w -> mem !! w) bv

edge :: Memory -> BitVector -> Bool
edge Memory{updated=[]} _  = False
edge Memory{updated=update:_} (BitVector bv) = isJust $ find ((`elem` wbv) . fst) update
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

type UnaryBVFun = BitVector -> BitVector -> Memory -> Memory

unaryFunc :: ([Bit] -> [Bit]) -> UnaryBVFun
unaryFunc f a y mem =
  let to = extend (bvLength y)
      ab = to $ mem ! a
   in mem // (y, to $ f ab)

-- | Logical Not
(!|) :: UnaryBVFun
(!|) = unaryFunc logicalNot

logicalNot :: [Bit] -> [Bit]
logicalNot a = [complement (or a)]

-- | Negation
(-|) :: UnaryBVFun
(-|) = unaryFunc neg

neg :: [Bit] -> [Bit]
neg = inc . bitwiseNot

inc :: [Bit] -> [Bit]
inc = reverse . fst . foldl (\(n, c) a -> (a `xor` c : n, a .&. c)) ([], H)

-- | Bitwise Not
(~|) :: UnaryBVFun
(~|) = unaryFunc bitwiseNot

bitwiseNot :: [Bit] -> [Bit]
bitwiseNot = map not

-- | Positive
(+|) :: UnaryBVFun
(+|) = unaryFunc pos

pos :: [Bit] -> [Bit]
pos a = a

-- | Reduce And
(&/|) :: UnaryBVFun
(&/|) = unaryFunc andReduce

andReduce :: [Bit] -> [Bit]
andReduce a = [and a]

-- | Boolean Reduce
(!!|) :: UnaryBVFun
(!!|) = unaryFunc boolReduce

boolReduce :: [Bit] -> [Bit]
boolReduce = logicalNot . logicalNot

-- | Reduce OR
(|/|) :: UnaryBVFun
(|/|) = unaryFunc orReduce

orReduce :: [Bit] -> [Bit]
orReduce a = [or a]

-- | Reduce XNOR
(!^/|) :: UnaryBVFun
(!^/|) = unaryFunc xnorReduce

xnorReduce :: [Bit] -> [Bit]
xnorReduce a = [all not a .|. and a]

-- | Reduce XOR
(^/|) :: UnaryBVFun
(^/|) = unaryFunc xorReduce

xorReduce :: [Bit] -> [Bit]
xorReduce a = [foldl1 xor a]

------------------------
--  Binary functions  --
------------------------

type BinaryBVFun = BitVector -> BitVector -> BitVector -> Memory -> Memory

binaryFunc :: ([Bit] -> [Bit] -> [Bit]) -> BinaryBVFun
binaryFunc f a b y mem =
  let toA = extend (max (bvLength a) (bvLength b))
      toR = extend (bvLength y)
      ab = toA $ mem ! a
      bb = toA $ mem ! b
  in mem // (y, traceShowId (toR $ ab `f` bb))

math :: (Int -> Int -> Maybe Int) -> ([Bit] -> [Bit] -> [Bit])
math f a b = fromMaybe [X] $ do
  a' <- toInt a
  b' <- toInt b
  fromInt <$> a' `f` b'

-- | Add
(|+|) :: BinaryBVFun
(|+|) = binaryFunc add

add :: [Bit] -> [Bit] -> [Bit]
add = math (\a b -> Just (a + b))

-- | And
(|&|) :: BinaryBVFun
(|&|) = binaryFunc and'

and' :: [Bit] -> [Bit] -> [Bit]
and'= zipWith (.&.)

-- | Bitwise case equal

(|===.|) :: BinaryBVFun
(|===.|) = binaryFunc bweqx

bweqx :: [Bit] -> [Bit] -> [Bit]
bweqx = zipWith (\a b -> fromBool (a == b))

-- | Div

(|//|) :: BinaryBVFun
(|//|) = binaryFunc div'

div' :: [Bit] -> [Bit] -> [Bit]
div' = math sDiv
  where
    sDiv _ 0 = Nothing
    sDiv a b = Just (div a b)

-- | Div Floor

{-# WARNING (|//.|) "Signed bitvectors are not supported, so this functions behaves exactly like div" #-}
(|//.|) :: BinaryBVFun
(|//.|) = binaryFunc floorDiv

floorDiv :: [Bit] -> [Bit] -> [Bit]
floorDiv _a _b = undefined

-- | Mod

modB :: BinaryBVFun
modB = binaryFunc mod'

mod' :: [Bit] -> [Bit] -> [Bit]
mod' = math sMod
  where
    sMod _ 0 = Nothing
    sMod a b = Just (a `mod` b)

-- | Equality

(|==|) :: BinaryBVFun
(|==|) = binaryFunc eq

eq :: [Bit] -> [Bit] -> [Bit]
eq = (. andReduce) . zipWith equal

-- | Case equality

(|===|) :: BinaryBVFun
(|===|) = binaryFunc caseEq

{-# WARNING caseEq "Implement case equality" #-}
caseEq :: [Bit] -> [Bit] -> [Bit]
caseEq = (. andReduce) . zipWith (\a b -> fromBool (a == b))

-- | Greater Equal

(|>=|) :: BinaryBVFun
(|>=|) = binaryFunc greaterEqual

greaterEqual :: [Bit] -> [Bit] -> [Bit]
greaterEqual = math (\a b -> Just $ fromEnum (a >= b))

-- | Greater than

(|>|) :: BinaryBVFun
(|>|) = binaryFunc greaterThan

greaterThan :: [Bit] -> [Bit] -> [Bit]
greaterThan = math (\a b -> Just $ fromEnum (a > b))

-- | Less Equal

(|<=|) :: BinaryBVFun
(|<=|) = binaryFunc lessEqual

lessEqual :: [Bit] -> [Bit] -> [Bit]
lessEqual = math (\a b -> Just $ fromEnum (a <= b))

-- | Less Than

(|<|) :: BinaryBVFun
(|<|) = binaryFunc lessThan

lessThan :: [Bit] -> [Bit] -> [Bit]
lessThan = math (\a b -> Just $ fromEnum (a < b))

-- | Logic And

(|&&|) :: BinaryBVFun
(|&&|) = binaryFunc lAnd

lAnd :: [Bit] -> [Bit] -> [Bit]
lAnd a b = boolReduce (a `and'` b)

-- | Logic Or

(||||) :: BinaryBVFun
(||||) = binaryFunc lOr

lOr :: [Bit] -> [Bit] -> [Bit]
lOr a b = boolReduce (a `or'` b)

-- | Or

(|||) :: BinaryBVFun
(|||) = binaryFunc or'

or' :: [Bit] -> [Bit] -> [Bit]
or' = zipWith (.|.)

-- | XNor

(|!^|) :: BinaryBVFun
(|!^|) = binaryFunc xnor

xnor :: [Bit] -> [Bit] -> [Bit]
xnor = zipWith ((. not) . xor)

-- | Xor

(|^|) :: BinaryBVFun
(|^|) = binaryFunc xor'

xor' :: [Bit] -> [Bit] -> [Bit]
xor' = zipWith xor

------------------
-- Multeplexers --
------------------

bmux :: BitVector -> BitVector -> BitVector -> Memory -> Memory
bmux a s _y mem =
  let _ab = mem ! a
      _sb = mem ! s
  in undefined

bwmux :: BitVector -> BitVector -> BitVector -> BitVector -> Memory -> Memory
bwmux a b s y mem =
  let ab = mem ! a
      bb = mem ! b
      sb = mem ! s
      res = zipWith3 (\case H -> \a' _ -> a'; L -> \_ b' -> b'; _ -> \_ _ -> X) sb ab bb
  in mem // (y, res)

demux :: BitVector -> BitVector -> BitVector -> Memory -> Memory
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

mux :: BitVector -> BitVector -> BitVector -> BitVector -> Memory -> Memory
mux a b s y mem =
  let ab = mem ! a
      bb = mem ! b
      [ s' ] = mem ! s
      res = case s' of H -> bb; L -> ab; _ -> replicate (length ab) Z
  in mem // (y, res)

tribuf :: BitVector -> BitVector -> BitVector -> Memory -> Memory
tribuf a en y mem =
  let ab = mem ! a
      [ en' ] = mem ! en
  in mem // (y, case en' of H -> ab; L -> replicate (length ab) Z; _ -> replicate (length ab) X)

---------------
-- Registers --
---------------

adff :: Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector -> Memory -> Memory
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

adffe :: Bit -> Bit -> Bit -> BitVector
      -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector
      -> Memory -> Memory
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

adlatch :: Bit -> Bit -> BitVector
        -> BitVector -> BitVector -> BitVector -> BitVector
        -> Memory -> Memory
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

aldff :: Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector -> Memory -> Memory
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

aldffe :: Bit -> Bit -> Bit -> BitVector
      -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector
      -> Memory -> Memory
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

dff :: Bit -> BitVector -> BitVector -> BitVector -> Memory -> Memory
dff clkPol clk d q mem =
  let [ clk' ] = mem ! clk
   in if clk' `equal'` clkPol && edge mem clk then
    mem // (q, mem ! d)
  else
    mem

dffe :: Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> Memory -> Memory
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

sdff :: Bit -> Bit -> BitVector -> BitVector -> BitVector -> BitVector -> BitVector -> Memory -> Memory
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

