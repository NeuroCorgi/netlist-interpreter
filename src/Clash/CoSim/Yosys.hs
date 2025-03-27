{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module Clash.CoSim.Yosys
  ( externalComponent
  , externalComponentE
  , defaultOptions
  , ModuleOptions(..)
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

import Control.Arrow (first, (&&&))
import Control.Monad (mapAndUnzipM)

import Data.Traversable (mapAccumM)
import Data.Maybe (fromJust)
import qualified Data.Map as Map (Map, fromList, lookup, (!))
import qualified Data.List as List

import Data.Vector (Vector)
import qualified Data.Vector as V (singleton)

import Clash.Annotations.Primitive (PrimitiveGuard(HasBlackBox))
import Clash.Prelude (pack)
import Clash.Signal.Internal (Signal((:-)))
import qualified Clash.Signal (KnownDomain, Signal, Clock, Reset, Enable, fromEnable, unsafeToActiveHigh)
import qualified Clash.Sized.BitVector (BitVector)

import qualified Intermediate as I
import qualified Memory as M
import qualified Interpreter as C
import qualified Util as U

import Internal.Util

import Clash.CoSim.Yosys.Internal
import Clash.CoSim.Yosys.Util

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
    state' = C.eval $ put (Map.fromList [(clockName, high)]) $ C.eval $ put ins $ C.eval $ put (Map.fromList [(clockName, low)]) state
  in
    -- Pull clock back down again, just in case
    (out, C.eval $ put (Map.fromList [(clockName, low)]) state')
  where
    put = flip C.start
    high = V.singleton M.H
    low = V.singleton M.L

---
-- Just wrapper functions to use in Template Haskell splice
-- so users do not need to import the wrapped functions themselves
compile :: I.Module -> C.Design
compile = U.compile

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
  { topEntityName :: String
  -- ^ Name of the top level entity in the design file
  , parameters :: [(String, String)]
  -- ^ Parameters to set for the module.
  -- The first element of a tuple is a name of a parameter, the second is a value.
  -- If the value of the parameter is a string, add escaped quotes at the beggining and the end of the string `[("param_name", "\"value\"")]`
  , ignoreCache :: Bool
  -- ^ Ignore cache and run yosys again
  , nonCombinatorialInputs :: [String]
  -- ^ Input ports that are known to not depend on the outputs of the design
  -- Default non-combinatorial inputs are clock and reset signals
  }

defaultOptions :: ModuleOptions
defaultOptions = ModuleOptions
  { topEntityName = "topEntity"
  , parameters = []
  , ignoreCache = False
  , nonCombinatorialInputs = [] }

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
$( externalModule "synch.v" defaultOptions{ topEntityName="foo" } )
```
would have type signature like this
```haskell
foo :: Clash.Signal.KnownDomain dom => Clash.Signal.Clock dom -> Clash.Signal.Enable dom -> Clash.Signal.Signal dom (Clash.Sized.BitVector 4) -> Clash.Signal.Signal dom (Clash.Sized.BitVector 1)
```

And with "test.v" being
```verilog
module topEntity(a, b, c);
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
topEntity :: Clash.Sized.BitVector 2 -> Clash.Sized.BitVector 1 -> Clash.Sized.BitVector 3
```

-}
externalComponent ::
  FilePath ->
  -- ^ Path to the verilog design file
  ModuleOptions ->
  -- ^ Options
  Q [Dec]
externalComponent filePath ModuleOptions{topEntityName=topLevelName, parameters=moduleParameters, ignoreCache=readFromCache, nonCombinatorialInputs=nonCombIns} = do
  topLevel <- runIO $ U.readDesign filePath (not readFromCache) topLevelName moduleParameters
  let name = mkName $ I.modName topLevel
      markedInputs = markRolesForInputs topLevel
      markedOutputs = map (, Other) $ I.modOutputs topLevel
      maybeClockName = List.find ((== Clock) . snd) markedInputs
      specialSignalNames = map (I.pName . fst) $ filter ((/= Other) . snd) markedInputs
      markedDependentGroups = markDependentOutputs topLevel (specialSignalNames ++ nonCombIns)
      -- There should be only one non-dependent group
      --TODO: check if there's truly only one such group?
      ([nonDependent], dependentGroups) = both uniteDependentGroups $ List.partition ((== NotDependent) . snd) markedDependentGroups

  initStateName <- newName "initState"
  let initD = valD (varP initStateName) (normalB [| compile $(liftData topLevel) |]) []

  noinlineAnn <- pragInlD name NoInline FunLike AllPhases
  hasBlackBoxAnn <- pragAnnD (ValueAnnotation name) [| (HasBlackBox [] ()) |]
  -- If there is something that resembles clock signal, then consider the design synchronous
  case maybeClockName of
    -- clock name is needed to tick it during update, separately from all other inputs
    Just clockName -> do
      let dom = mkName "dom"
          reorderedInputs = reorder markedInputs
          (inputPatterns, acss, argTys) = convertInputs (Just dom) reorderedInputs
          outTy = convertOutputs (Just dom) markedOutputs
          inputNameMap = Map.fromList $ zip (map (I.pName . fst) markedInputs) (map fst acss)
      state <- newName "stau29te"
      out <- newName "out"
      go <- newName "go"

      (nonDepOutStateName, nonDepGroupEval) <- do
        (outputMapName, func) <- evalGroup []
        pure (map (, outputMapName) (fst nonDependent), func initStateName)

      (depGroupsOutStateNames, depGroupsEval) <- mapAndUnzipM
        (\(outs, dependencies -> deps) -> do
            (outputMapName, func) <- evalGroup (map (id &&& (inputNameMap Map.!)) deps)
            pure (map (, outputMapName) outs, func)
        ) dependentGroups

      (lastStateName, dependentEval) <- mapAccumM (\prevStateName func -> func prevStateName) initStateName depGroupsEval

      let outStateNameMap = Map.fromList $ map (first I.pName) $ concat (nonDepOutStateName : depGroupsOutStateNames)
      -- `drop 1` in the declaration of `dec` drops the clock signal (which is the first after reordering)
      -- clock signal is not a real signal and is useless for the purpose of design evaluation
      dec <- funD name
        [clause inputPatterns (normalB (foldl appE [| $(varE go) $(varE initStateName) |] $ drop 1 $ map (varE . fst) acss) )
        -- where
          [ initD
          , funD go
            [ clause ((varP state) : map ((\(f, s) -> [p| ~($f :- $s) |]) . both varP) (drop 1 acss))
              (normalB
                [|
                  let
                    mapping = $(makeMapping $ drop 1 $ zip (map (I.pName . fst) reorderedInputs) (map fst acss))
                    ($(varP out), state') = tick $(liftString $ I.pName $ fst clockName) $(varE state) mapping
                    result = $(makeUnmapping $ map ((, out) . I.pName . fst) markedOutputs)
                  in
                    result :- $(varE lastStateName) `seq` $(foldl appE [| $(varE go) state' |] $ drop 1 $ map (varE . snd) acss)
                 |]) [] ]
          ] ]
      sig <- (sigD name [t| Clash.Signal.KnownDomain $(varT dom) => $(makeArrow argTys outTy) |])
      return [sig, dec, noinlineAnn, hasBlackBoxAnn]
    _ -> do
      let (inputPatterns, acss, argTys) = convertInputs Nothing markedInputs
          outTy = convertOutputs Nothing markedOutputs
          inputNameMap = Map.fromList $ zip (map (I.pName . fst) markedInputs) (map fst acss)

      (groupsOutStateNames, groupsEval) <- mapAndUnzipM
        (\(outs, dependencies -> deps) -> do
            (outputMapName, func) <- evalGroup (map (id &&& (inputNameMap Map.!)) deps)
            pure (map (, outputMapName) outs, func)
        ) (nonDependent : dependentGroups)

      (_, dependentEval) <- mapAccumM (\prevStateName func -> func prevStateName) initStateName groupsEval

      let outStateNameMap = Map.fromList $ map (first I.pName) $ concat groupsOutStateNames
      dec <- funD name
        [clause inputPatterns
         (normalB $
           letE
           (map pure $ concat dependentEval )
           (makeUnmapping (map ((id &&& (outStateNameMap Map.!)) . I.pName . fst) markedOutputs))
         )
         [ initD ] ]
      sig <- sigD name (makeArrow argTys outTy)
      return [sig, dec, noinlineAnn, hasBlackBoxAnn]
  where
    evalGroup :: [(String, Name)] -> Q (Name, Name -> Q (Name, [Dec]))
    evalGroup inputs = do
      outName <- newName "stateOutput"
      pure
        (outName, \prevStateName -> do
            stateName <- newName "state"
            (stateName ,) <$>
              [d|
                sourceInputs = $(makeMapping inputs)
                (_, $(varP stateName)) = tick "" $(varE prevStateName) sourceInputs
                $(varP outName) = peek $(varE stateName)
                |])

    makeArrow argTys retTy = foldr (appT . appT arrowT) retTy argTys

    makeMapping inputs = [| mapFromList $(listE $ map (\(p, a) -> [| ($(liftString p), toVectorBit $(varE a)) |]) inputs) |]

    makeUnmapping [output] = makeSingleUnmap output
    makeUnmapping outputs = tupE $ map makeSingleUnmap outputs
    makeSingleUnmap (output, out) = [| fromVectorBit (mapLookup $(varE out) $(liftString output)) |]

{- | Same as `externalComponent` but an expression, so inputs can be easily packed and unpacked for use

Continuing the second example from `externalComponent`,
```haskell
foo :: Unsigned 2 -> Bit -> Unsigned 3
foo a b = unpack $ $( externalComponentE "test.v" defaultOptions ) (pack a) (pack b)
```

-}
externalComponentE ::
  FilePath ->
  -- ^ Path to the verilog design
  ModuleOptions ->
  -- ^ Options
  Q Exp
externalComponentE filePath modOptions = do
  decs@(SigD name _ : _) <- externalComponent filePath modOptions
  -- Take only the signature and the declaration, as annotations are not allowed in `let`
  letE (map pure $ take 2 decs) (varE name)


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
convertPort (Just d) (I.Port{..}, Clock) = (varP n, (acs, acs), [t| Clash.Signal.Clock $(varT d) |])
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
    (names, ress) = unzip $ map (\(_, (name, _), c) -> (name, c)) $ convertPorts Nothing outs

    -- If there are multiple return values, construct a tuple
    ty = case ress of
      [] -> tupleT 0 -- unit type
      [single] -> single
      multiple -> foldl appT (tupleT (length multiple)) multiple
