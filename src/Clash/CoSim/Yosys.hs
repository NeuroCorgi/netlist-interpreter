{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module Clash.CoSim.Yosys
  ( externalComponent
  , externalComponentE
  , defaultOptions
  , tick
  , toVectorBit
  , fromVectorBit
  -- * Functions to make template haskell happy
  , mapFromList
  , mapLookup
  , maybeFromJust
  , PrimitiveGuard(HasBlackBox)
  , zipLongest
  , bits
  )
where

import Language.Haskell.TH hiding (Role)
import Language.Haskell.TH.Syntax (liftString, liftData)

import Data.Maybe (mapMaybe, fromJust)
import Data.Either.Extra (fromRight')
import qualified Data.Map as Map (Map, fromList, lookup, (!))
import qualified Data.List as List (nub, partition, find)

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)

import Data.Function (on)

import Clash.Annotations.Primitive (PrimitiveGuard(HasBlackBox))
import Clash.Prelude (pack)
import Clash.Signal.Internal (Signal((:-)))
import Clash.Sized.Internal.BitVector (BitVector(..))
import qualified Clash.Signal (KnownDomain, Signal, Clock, Reset, Enable, fromEnable, unsafeToActiveHigh)
import qualified Clash.Sized.BitVector (BitVector)

import GHC.Num (Natural, integerToNatural)
import GHC.Natural (naturalToInteger)

import qualified Intermediate as I
import qualified Memory as M
import qualified Interpreter as C
import qualified Util as U

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | Tick one clock cycle in an asynchronous design
tick ::
  String ->
  -- ^ Name of the clock signal in the design
  C.Design ->
  -- ^ Evaluatable design
  Map.Map String (Vector M.Bit) ->
  -- ^ Mapping of other inputs to the design
  (Map.Map String (Vector M.Bit), C.Design)
tick clockName state ins =
  let
    -- Read outputs from previous input or from initial setup if it's the first clock cycle
    out = C.peek state
    -- Pull clock down, set all the values, pull clock up
    state' = C.eval $ put (Map.fromList [(clockName, [ M.H ])]) $ C.eval $ put ins $ C.eval $ put (Map.fromList [(clockName, [ M.L ])]) state
  in
    -- Pull clock back down again, just in case
    (out, C.eval $ put (Map.fromList [(clockName, [ M.L ])]) state')
  where
    put = flip C.start

toVectorBit :: BitVector n -> Vector M.Bit
toVectorBit (BV mask nat) = Vector.fromList $ map from $ ((zipLongest False) `on` (bits . naturalToInteger)) mask nat
  where
    from (True, True) = M.X
    from (True, False) = M.Z
    from (False, True) = M.H
    from (False, False) = M.L

bits :: Integer -> [Bool]
bits 0 = []
bits n
  | even n = False : bits nn
  | odd n = True : bits nn
  where nn = n `div` 2

zipLongest :: a -> [a] -> [a] -> [(a, a)]
zipLongest _ [] [] = []
zipLongest d [] (a : rest) = (d, a) : zipLongest d [] rest
zipLongest d (a : rest) [] = (a, d) : zipLongest d rest []
zipLongest d (a : as) (b : bs) = (a, b) : zipLongest d as bs

fromVectorBit :: Vector M.Bit -> BitVector n
fromVectorBit vect = let (mask, nat) = toNatPair vect in BV { unsafeMask = mask, unsafeToNatural = nat }
  where
    toNatPair vect = both integerToNatural $ foldr (\b (mask', value) -> (2 * mask' + mask b, 2 * value + val b)) (0 :: Integer, 0 :: Integer) vect
      where
        val M.H = 1
        val M.L = 0
        val _   = 0

        mask M.Z = 1
        mask M.X = 1
        mask _ = 0

---
-- Just wrapper functions to use in Template Haskell splice
-- so users do not need to import the wrapped functions themselves
compile :: I.Module -> C.Design
compile = U.compile

compile' :: C.Design
compile' = fromRight' $ Left ""

peek :: C.Design -> Map.Map String (Vector M.Bit)
peek = C.peek

mapFromList :: Ord k => [(k, v)] -> Map.Map k v
mapFromList = Map.fromList

mapLookup :: Ord k => Map.Map k v -> k -> v
mapLookup = (Map.!)

maybeFromJust :: Maybe a -> a
maybeFromJust = fromJust
---

data ModuleOptions = ModuleOptions
  { topEntity :: String
  , parameters :: [(String, String)]
  }

defaultOptions :: ModuleOptions
defaultOptions = ModuleOptions { topEntity = "topEntity", parameters = [] }

{- | Instantiates the provided verilog design.

The type signature is automatically generated.
If the design is synchronous, then
`Clash.Signal.Clock`, 'Clash.Signal.Reset`, and `Clash.Signal.Enable` signals are found and put at the beggining;
other inputs are represented with a `Clash.Sized.BitVector` signals with appropriate width.
Otherwise, all inputs are just `Clash.Sized.BitVector`s.

For example,

With "synch.v" being
```verilog
module foo(clk, en, a, y);
  input     clk;
  input      en;
  input [3:0] a;

  output      y;

  always @(posedge clk) begin ... end

  ...
endmodule
```
,
```haskell
$( externalModule "synch.v" defaultOptions{ topLevel="foo" } )
```
would have type signature like this
```haskell
topLevel :: Clash.Signal.KnownDomain dom => Clash.Signal.Clock dom -> Clash.Signal.Enable dom -> Clash.Signal.Signal dom (Clash.Sized.BitVector 4) -> Clash.Signal.Signal dom (Clash.Sized.BitVector 1)
```

And with "test.v" being
```verilog
module topLevel(a, b, c);
  input  [1:0] a;
  input        b;
  output [2:0] c;

  assign c = ...;

  ...
endmodule
```
,
```haskell
$( externalModule "test.v" defaultOptions )
```
would have type signature like this
```haskell
topLevel :: Clash.Sized.BitVector 2 -> Clash.Sized.BitVector 1 -> Clash.Sized.BitVector 3
```

-}
externalComponent ::
  FilePath ->
  -- ^ Path to the verilog design file
  ModuleOptions ->
  -- ^ Top level entity name
  Q [Dec]
externalComponent filePath ModuleOptions{topEntity=topLevelName, parameters=moduleParameters} = do
  -- Only the first element, which is a topLevel of the design, is of interesest
  -- as only its inputs and outputs are exposed
  topLevel <- runIO $ U.readDesign filePath topLevelName moduleParameters
  let name = mkName $ I.modName topLevel
      markedInputs = markInputs topLevel
      markedOutputs = map (, Other) $ I.modOutputs topLevel
      clockName = List.find ((== Clock) . snd) markedInputs
      init = mkName "initState"
      initD = valD (varP init) (normalB [| compile $(liftData topLevel) |]) []
      -- initD = valD (varP init) (normalB [| compile' |]) []
  noinlineAnn <- pragInlD name NoInline FunLike AllPhases
  hasBlackBoxAnn <- pragAnnD (ValueAnnotation name) [| (HasBlackBox [] ()) |]
  -- If there is something that resembles clock signal, then consider the design synchronous
  case clockName of
    -- clock name is needed to tick it during update, separately from all other inputs
    Just clockName -> do
      let dom = mkName "dom"
          reorderedInputs = reorder markedInputs
          (names, acss, argTys) = convertInputs (Just dom) reorderedInputs
          outTy = convertOutputs (Just dom) markedOutputs
          state = mkName "state"
          out = mkName "out"
          go = mkName "go"
      -- `drop 1` in the declaration of `dec` drops the clock signal (which is the first after reordering)
      -- clock signal is not a real signal and is useless for the purpose of design evaluation
      dec <- funD name
        [clause names (normalB (foldl appE [| $(varE go) $(varE init) |] $ drop 1 $ map (varE . fst) acss) )
        -- where
          [ initD
          , funD go
            [ clause ((varP state) : map ((\(f, s) -> [p| ($f :- $s) |]) . both varP) (drop 1 acss))
              (normalB
                [|
                  let
                    mapping = $(makeMapping $ drop 1 $ zip (map fst reorderedInputs) (map (varE . fst) acss))
                    ($(varP out), state') = tick $(liftString $ I.pName $ fst clockName) $(varE state) mapping
                    result = $(makeUnmapping (varE out) $ map fst markedOutputs)
                  in
                    result :- $(foldl appE [| $(varE go) state' |] $ drop 1 $ map (varE . snd) acss)
                 |]) [] ]
          ] ]
      sig <- sigD name [t| Clash.Signal.KnownDomain $(varT dom) => $(makeArrow argTys outTy) |]
      return [sig, dec, noinlineAnn, hasBlackBoxAnn]
    _ -> do
      let (names, acss, argTys) = convertInputs Nothing markedInputs
          outTy = convertOutputs Nothing markedOutputs
          out = mkName "out"
      dec <- funD name
        [clause names
         (normalB
           [|
            let
              mapping = $(makeMapping $ zip (map fst markedInputs) (map (varE . fst) acss))
              -- There is no clock in an asynchonous design
              (_, state) = tick "" $(varE init) mapping
              $(varP out) = peek state
              result = $(makeUnmapping (varE out) $ map fst markedOutputs)
            in
              result
            |])
         [ initD ] ]
      sig <- sigD name (makeArrow argTys outTy)
      return [sig, dec, noinlineAnn, hasBlackBoxAnn]
  where
    makeArrow argTys retTy = foldr (appT . appT arrowT) retTy argTys

    makeMapping inputs = [| mapFromList $(listE $ map (\(p, a) -> [| ($(liftString $ I.pName p), toVectorBit $a) |]) inputs) |]

    makeUnmapping out [output] = makeSingleUnmap out output
    makeUnmapping out outputs = tupE $ map (makeSingleUnmap out) outputs
    makeSingleUnmap out output = [| fromVectorBit (mapLookup $out $(liftString $ I.pName output)) |]

{- | Same as `externalComponent` but an expression, so ports can be easily packed and unpacked for use

Continuing the second example from `externalComponent`,
```haskell
foo :: Unsigned 2 -> Bit -> Unsigned 3
foo a b = unpack $ $(externalComponentE "test.v" def) (pack a) (pack b)
```

-}
externalComponentE ::
  FilePath ->
  -- ^ Path to the verilog design
  ModuleOptions ->
  -- ^ Top level entity name
  Q Exp
externalComponentE filePath modOptions = do
  decs@(SigD name _ : _) <- externalComponent filePath modOptions
  -- Take only the signature and the declaration, as annotations are not allowed in `let`
  letE (map pure $ take 2 decs) (varE name)

data Role
  = Clock
  | Reset
  | Enable
  | Other
  deriving Eq

markInputs :: I.Module -> [(I.Port, Role)]
-- Multy domain design with several clock signals?
markInputs I.Module{I.modInputs, I.modCells} = map mark modInputs
  where
    -- Naive guesses for names of ports
    clock, reset, enable :: [(String -> Bool)]
    clock = [(== "clk"), (== "clock")]
    reset = [(== "rst"), (== "arst"), (== "reset")]
    enable = [(== "en"), (== "enable")]

    mark p@I.Port{I.pName, I.pBits}
    -- First naive guess by the port name
      | like clock pName = (p, Clock)
      | like reset pName = (p, Reset)
      | like enable pName = (p, Enable)
    -- Second guess by loking at internal cell inputs
      | cs == cellClockInputs = (p, Clock)
      | cs == cellResetInputs = (p, Reset)
      | cs == cellEnableInputs = (p, Enable)
      | otherwise = (p, Other)
      where cs = M.wires pBits

    like f name = or $ f <*> return name

    cellInputs signal = List.nub . concatMap M.wires . mapMaybe (\I.Cell{cConnections} -> Map.lookup signal cConnections)
    cellClockInputs = cellInputs "CLK" modCells
    cellResetInputs = cellInputs "ARST" modCells
    cellEnableInputs = cellInputs "EN" modCells

reorder :: [(a, Role)] -> [(a, Role)]
reorder lst = clock ++ reset ++ enable ++ rest''
  where
    (clock, rest) = List.partition ((== Clock) . snd) lst
    (reset, rest') = List.partition ((== Reset) . snd) rest
    (enable, rest'') = List.partition ((== Enable) . snd) rest'

convertPort ::
  Maybe Name ->
  -- ^ Name of a domain if a port is a part of a synchronous design
  (I.Port, Role) ->
  -- ^ Port with its attached role
  (Q Pat, (Name, Name), Q Type)
  -- ^ Pattern to match a signal (or correctly deconstruct it into a signal in case of clock, reset, or enable),
  --   Names of a head and a continuation of the signal
  --   Type of the signal

-- Clock pattern is useless as its dropped right after
convertPort (Just d) (I.Port{..}, Clock) = ((varP n), (acs, acs), [t| Clash.Signal.Clock $(varT d) |])
  where acs = n
        n = mkName pName
convertPort (Just d) (I.Port{..}, Reset) =
  ([p| (fmap pack . Clash.Signal.unsafeToActiveHigh -> $(varP rstName)) |],
   (rstName, rstSig),
   [t| Clash.Signal.Reset $(varT d) |])
  where rstName = mkName pName
        rstSig = mkName (pName ++ "_tail")
convertPort (Just d) (I.Port{..}, Enable) =
  ([p| (fmap pack . Clash.Signal.fromEnable -> $(varP enName)) |],
    (enName, enSig),
    [t| Clash.Signal.Enable $(varT d) |])
  where enName = mkName pName
        enSig = mkName (pName ++ "_tail")
convertPort dom (I.Port{I.pName=portName, I.pBits=bits}, _)
  | Just d <- dom = (varP sigHead, (sigHead, sigTail), bv2signal d)
  | otherwise = (varP sigHead, (sigHead, sigHead), bv2type)
  where
    sigHead = mkName portName
    sigTail = mkName (portName ++ "_tail")

    bv2type = [t| Clash.Sized.BitVector.BitVector $(fromIntegral $ length bits) |]
    bv2signal d = [t| Clash.Signal.Signal $(varT d) $bv2type |]

convertPorts :: Maybe Name -> [(I.Port, Role)] -> [(Q Pat, (Name, Name), Q Type)]
convertPorts dom = map (convertPort dom)

convertInputs :: Maybe Name -> [(I.Port, Role)] -> ([Q Pat], [(Name, Name)], [Q Type])
convertInputs dom ins = (names, acss, args)
  where
    (names, acss, args) = unzip3 $ convertPorts dom ins
    -- pat = map (\n -> (n, mkName n)) names

convertOutputs :: Maybe Name -> [(I.Port, Role)] -> Q Type
convertOutputs dom outs
  | Just dom <- dom = [t| Clash.Signal.Signal $(varT dom) $ty |]
  | otherwise = ty
  where
    -- Discard patterns and accessor names as they are not needed for the output
    ress = map (\(_, _, c) -> c) $ convertPorts Nothing outs

    -- If there are multiple return values, construct a tuple
    ty = case ress of
      [single] -> single
      multiple -> foldl appT (tupleT (length multiple)) multiple
