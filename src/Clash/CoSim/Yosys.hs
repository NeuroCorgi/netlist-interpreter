{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveLift #-}

{-# OPTIONS_GHC -Wno-gadt-mono-local-binds #-}

module Clash.CoSim.Yosys
  ( externalComponent
  , externalComponentE
  , defaultOptions
  , ModuleOptions(..)
  , tick
  , toVectorBit
  , fromVectorBit
  , clockTicks
  , Edge(..)
  , edgeToState
  -- * Functions to make template haskell happy
  , mapFromListMaybes
  , mapLookup
  , maybeFromJust
  , PrimitiveGuard(HasBlackBox)
  , zipLongest
  , bits
  , high
  , low
  , unzipFN
  )
where

import Language.Haskell.TH hiding (Role)
import Language.Haskell.TH.Syntax (lift, liftString, liftData, Lift)

import Control.Arrow (first, (&&&))
import Control.Monad (mapAndUnzipM, (<=<))

import Data.Int (Int64)
import Data.Coerce (coerce)

import Data.Traversable (mapAccumM)
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import qualified Data.Map as Map (Map, fromList, empty, (!))
import qualified Data.List as List
import Data.List.NonEmpty (toList, unfoldr, NonEmpty((:|)))

import Data.Vector (Vector)
import qualified Data.Vector as V (singleton)

import Data.Function (on)

import Clash.Annotations.Primitive (PrimitiveGuard(HasBlackBox))
import Clash.Prelude (pack)
import Clash.Signal.Internal (Signal((:-)), Femtoseconds(..), SDomainConfiguration(..), knownDomain)
import qualified Clash.Signal.Internal as CLK (Clock(..))
import qualified Clash.Signal (KnownDomain, Signal, Clock, Reset, Enable, fromEnable, fromList, unsafeToActiveHigh, sample)
import qualified Clash.Sized.BitVector (BitVector)

import Clash.Promoted.Symbol (ssymbolToString)
import Clash.Promoted.Nat (snatToNum)

import qualified Intermediate as I
import qualified Memory as M
import qualified Interpreter as C
import qualified Util as U

import Internal.Util

import Clash.CoSim.Yosys.Internal
import Clash.CoSim.Yosys.Util

data Edge = Rising | Falling deriving (Eq, Show, Lift)

oppositeEdge :: Edge -> Edge
oppositeEdge Rising = Falling
oppositeEdge Falling = Rising

class ClockTick t where
  ticks' :: [[Int64]] -> t

instance ClockTick [[(Int, Edge)]] where
  ticks' rawClocks = go clocks
    where
      -- Clocks are prepended to the list, reverse it to not get confused with clock domain indexes
      clocks = List.zip4 [0,1..] (reverse rawClocks) (repeat (0 :: Int64)) (repeat Rising)

      go clocks =
        let m = List.minimum $ map $(accs 4 2) clocks
            (a, b) = List.partition ((== m) . $(accs 4 2)) clocks
        in map ($(accs 4 0) &&& $(accs 4 3)) a : go (map tickDom a ++ b)
        where
          tickDom (ind, t : tr, absT, edge) = (ind, tr, absT + t, oppositeEdge edge)
          tickDom (_, [], _, _) = error "clock periods should never exhaust"

instance (ClockTick t, Clash.Signal.KnownDomain dom) => ClockTick (Clash.Signal.Clock dom -> t) where
  ticks' accumClocks clk
    | Just periods <- CLK.clockPeriods clk = ticks' (Clash.Signal.sample (unFemtoSeconds periods) : accumClocks)
    | SDomainConfiguration{sPeriod} <- knownDomain @dom = ticks' ((repeat . (* 1000) $ snatToNum sPeriod) : accumClocks)
    where
      unFemtoSeconds :: Signal dom Femtoseconds -> Signal dom Int64
      unFemtoSeconds = coerce

clockTicks :: (ClockTick t, Clash.Signal.KnownDomain dom) => Clash.Signal.Clock dom -> t
clockTicks = ticks' []

-- | Tick one clock cycle in an asynchronous design
tick ::
  String ->
  -- ^ Name of the clock signal in the design
  Vector M.Bit ->
  -- ^ Clock polarity to set
  C.Design ->
  -- ^ Evaluatable design
  Map.Map String (Vector M.Bit) ->
  -- ^ Mapping of other inputs to the design
  (Map.Map String (Vector M.Bit), C.Design)
tick clockName clockPol state ins =
  let
    state' = C.eval $ put (mapFromList [(clockName, clockPol)]) $ C.eval $ put ins state
    out = C.peek state'
  in
    (out, state')
  where
    put = flip C.start

high, low :: Vector M.Bit
high = V.singleton M.H
low  = V.singleton M.L

edgeToState :: Edge -> Vector M.Bit
edgeToState Rising = high
edgeToState Falling = low

---
-- Just wrapper functions to use in Template Haskell splice
-- so users do not need to import the wrapped functions themselves
compile :: I.Module -> C.Design
compile = U.compile

peek :: C.Design -> Map.Map String (Vector M.Bit)
peek = C.peek

mapFromList :: Ord k => [(k, v)] -> Map.Map k v
mapFromList = Map.fromList

mapFromListMaybes :: Ord k => [Maybe (k, v)] -> Map.Map k v
mapFromListMaybes = Map.fromList . catMaybes

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
$( externalModule [t| "clk" :: Clock -> "en" :: Enable -> "a" :: Signal (BitVector 4) -> "y" :: Signal (BitVector 1) |] "synch.v" defaultOptions{ topEntityName="foo" } )
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
$( externalModule [t| "a" :: BitVector 2 -> "b" :: BitVector 1 -> "c" :: BitVector 3 |] "test.v" defaultOptions )
```
would have type signature like this
```haskell
topEntity :: Clash.Sized.BitVector 2 -> Clash.Sized.BitVector 1 -> Clash.Sized.BitVector 3
```

-}
externalComponent
  :: ([(String, Int)], [(String, Int)])
  -- ^ Type annotation of a component with port names annotations
  -> FilePath
  -- ^ Path to the verilog design file
  -> ModuleOptions
  -- ^ Options
  -> Q [Dec]
externalComponent (argTyNameDom, retTyNameDom) filePath ModuleOptions{topEntityName=topLevelName, parameters=moduleParameters, ignoreCache=readFromCache, nonCombinatorialInputs=nonCombIns} = do
  topLevel <- runIO $ U.readDesign filePath (not readFromCache) topLevelName moduleParameters

  let markedInputs = markRolesForInputs topLevel
      markedOutputs = map (, Other) $ I.modOutputs topLevel
      maybeClockName = List.find ((== Clock) . snd) markedInputs
      (argTyNames, argDoms) = map fst &&& Map.fromList $ argTyNameDom
      (retTyNames, retDoms) = map fst &&& Map.fromList $ retTyNameDom
      specialSignalNames = map (I.pName . fst) $ filter ((== Clock) . snd) markedInputs
      markedDependentGroups = markDependentOutputs topLevel (specialSignalNames ++ nonCombIns)
      (nonDependent, dependentGroups) = both uniteDependentGroups $ List.partition ((== NotDependent) . snd) markedDependentGroups
      allDependentInputs = concatMap (dependencies . snd) dependentGroups
      otherInputs = filter (`notElem` allDependentInputs) $ map (I.pName . fst) markedInputs
      mods = splitModule topLevel (specialSignalNames ++ nonCombIns)
      reorderedModules = reorderModsTo retTyNames mods

  name <- newName $ I.modName topLevel
  initStateName <- newName "initState"
  -- let initD = valD (varP initStateName) (normalB [| undefined |]) []
  let initD = valD (varP initStateName) (normalB [| compile $(liftData topLevel) |]) []

  -- noinlineAnn <- pragInlD name NoInline FunLike AllPhases
  -- hasBlackBoxAnn <- pragAnnD (ValueAnnotation name) [| (HasBlackBox [] ()) |]
  -- If there is something that resembles clock signal, then consider the design synchronous
  case maybeClockName of
    Just _ -> do
      let dom = mkName "dom"
          reorderedInputs = reorderTo argTyNames markedInputs
          reorderedOutputs = reorderTo retTyNames markedOutputs
          (inputPatterns, acss, argTys) = convertInputs (Just dom) reorderedInputs
          outTy = convertOutputs (Just dom) reorderedOutputs

          depMap = Map.fromList (map (, NotDependentPort) otherInputs ++ map (, DependentPort) allDependentInputs)

          inputPorts = zipWith
            (\(I.pName -> name, role) (head, tail) -> PortTH name role (argDoms Map.! name) (depMap Map.! name) head tail)
            reorderedInputs acss
          outputPorts = map (\(I.pName -> name, role) -> PortTH name role (retDoms Map.! name) DependentPort (mkName "") (mkName "")) reorderedOutputs

          portsMap = Map.fromList $ map (portName &&& id) (inputPorts ++ outputPorts)

          clockInputPorts = List.sortBy (compare `on` portDomain) $ filter ((== Clock) . portRole) inputPorts

      clockEvents <- newName "ticks"
      clockNames <- newName "names"
      dec <- funD name
        [ clause inputPatterns (normalB $ tupE' $ map (signal <=< moduleToTH clockEvents clockNames portsMap) reorderedModules)
          [ valD (varP clockEvents) (normalB $ foldl appE [| clockTicks |] $ map (varE . headAccessor) clockInputPorts ) []
          , valD (varP clockNames) (normalB $ listE $ map (liftString . portName) clockInputPorts) []
          ]
        ]
      sig <- sigD name [t| Clash.Signal.KnownDomain $(varT dom) => $(makeArrow argTys outTy) |]
      return [sig, dec]
    _ -> do
      let (inputPatterns, acss, argTys) = convertInputs Nothing $ reorderTo argTyNames markedInputs
          reorderedOutputs = reorderTo retTyNames markedOutputs
          outTy = convertOutputs Nothing reorderedOutputs

          inputNames = map (I.pName . fst) markedInputs
          otherInputs = filter (`notElem` allDependentInputs) inputNames
          inputNameMap = Map.fromList $ zip inputNames (map fst acss)
          dummyDomainEvent = triple (const 0) (const Rising)

      stateND <- newName "stateND"
      -- If there is no non dependent inputs, it does nothing
      nonDepInputsSet <- [d| (_, $(varP stateND)) = tick "" low $(varE initStateName) $(makeMapping Nothing (map (id &&& dummyDomainEvent (inputNameMap Map.!)) otherInputs)) |]

      (groupsOutStateNames, groupsEval) <- mapAndUnzipM
        (\(outs, dependencies -> deps) -> do
            (outputMapName, func) <- evalGroup Nothing (map (id &&& dummyDomainEvent (inputNameMap Map.!)) deps)
            pure (map (, outputMapName) outs, func)
        ) (nonDependent ++ dependentGroups)

      (_resultingState, groupsEvalDec) <- mapAccumM (\prevStateName func -> func prevStateName) stateND groupsEval

      let outStateNameMap = Map.fromList $ map (first I.pName) $ concat groupsOutStateNames
      dec <- funD name
        [clause inputPatterns
         (normalB
           (letE
            (map pure $ concat (nonDepInputsSet : groupsEvalDec) )
            (makeUnmapping (map ((id &&& (outStateNameMap Map.!)) . I.pName . fst) reorderedOutputs)))
         )
         [ initD ]
        ]
      sig <- sigD name (makeArrow argTys outTy)
      return [sig, dec]
  where
    tupE' [single] = single
    tupE' multi = tupE multi

    withInstNormalB inst body = guardedB
      [ (,) <$> normalG [| clashSimulation |] <*> body
      , (,) <$> normalG [| otherwise |] <*> inst
      ]

    evalGroup :: Maybe Name -> [(String, (Int, Edge, Name))] -> Q (Name, Name -> Q (Name, [Dec]))
    evalGroup ticks inputs = do
      outName <- newName "stateOutput"
      pure
        (outName, \prevStateName -> do
            stateName <- newName "state"
            (stateName ,) <$>
              [d|
                sourceInputs = $(makeMapping ticks inputs)
                ($(varP outName), $(varP stateName)) = tick "" low $(varE prevStateName) sourceInputs
                |])

    makeArrow argTys retTy = foldr (appT . appT arrowT) retTy argTys

    makeMapping :: Maybe Name -> [(String, (Int, Edge, Name))] -> ExpQ
    makeMapping currentEventName inputs =
      [| mapFromListMaybes
         $(listE $ map
            (\(portName, (portDomain, portComb, a)) ->
                [| if $(maybe [| True |] (\eventName -> [| ($(lift portDomain), $(lift portComb)) `elem` $(varE eventName) |]) currentEventName) then
                     Just ($(liftString portName), toVectorBit $(varE a))
                   else Nothing
                 |]) inputs) |]

    -- makeUnmapping [output] = makeSingleUnmap output
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
  ([(String, Int)], [(String, Int)]) ->
  -- ^ Type of the component with port name annotations
  FilePath ->
  -- ^ Path to the verilog design
  ModuleOptions ->
  -- ^ Options
  Q Exp
externalComponentE types filePath modOptions = do
  decs@(SigD name _ : _) <- externalComponent types filePath modOptions
  -- Take only the signature and the declaration, as annotations are not allowed in `let`
  letE (map pure $ take 2 decs) (varE name)

data PortDependency = DependentPort | NotDependentPort deriving (Eq, Show)

data PortTH = PortTH
  { portName :: String
  , portRole :: Role
  , portDomain :: Int
  , portDependency :: PortDependency
  , headAccessor :: Name
  , tailAccessor :: Name
  }
  deriving Show

destructivePattern :: PortTH -> Q Pat
destructivePattern PortTH{headAccessor, tailAccessor} =
  [p| ~($(varP headAccessor) :- $(varP tailAccessor)) |]

newtype ModuleTH = ModuleTH { signal :: Q Exp }

moduleToTH :: Name -> Name -> Map.Map String PortTH -> I.Module -> Q ModuleTH
moduleToTH clockEventsName clockNameMap portMap mod@I.Module{I.modInputs=inputs, I.modOutputs=outputs} = do
  output <- case outputs of
    [output] -> pure (portMap Map.! I.pName output)
    _ -> error "separate modules expected to have only one output"

  let part = I.modName mod ++ "_part"

  initName <- newName part
  initFunc <- newName $ "goInit_" ++ part
  cycleFunc <- newName $ "goCycle_" ++ part

  state <- newName "state"
  currentClockEvents <- newName "clockEvents"
  nextClockEvents <- newName "nextClockEvents"

  let allPortInputs = map ((portMap Map.!) . I.pName) inputs
      (_, portInputs) = List.partition ((== Clock) . portRole) allPortInputs

  clockTickOutMap <- newName "outMap"
  clockTickState <- newName "clockOutState"
  outState <- newName "outState"
  outMap <- newName "outMap"
  tempState <- newName "tempState"

  let
    outPortName = portName output
    initState = valD (varP initName) (normalB [| compile $(liftData mod) |]) []
    -- initState = valD (varP initName) (normalB [| undefined |]) []

    goPattern = (varP state : map destructivePattern portInputs)
    goInit = funD initFunc
      [ clause goPattern
        (normalB $
          letE
          [ valD [p| ($(varP outMap), $(varP tempState)) |] (normalB [| tick "" low $(varE state) $(makeMapping Nothing $ filter (const True . (== NotDependentPort) . portDependency) portInputs) |]) []
          , valD [p| (_, $(varP outState)) |] (normalB [| tick "" low $(varE tempState) $(makeMapping Nothing $ filter ((== DependentPort) . portDependency) portInputs) |]) []
          ]
          [| ($(makeUnmapping outMap outPortName), [(i, Rising) | i <- [0,1..]]) :
             $(foldl appE [| $(varE cycleFunc) $(varE clockEventsName) $(varE tempState) |] $ map (varE . tailAccessor) portInputs)
           |]
        ) [] ]
    goCycle = funD cycleFunc
      [ clause ([p| ($(varP currentClockEvents) : $(varP nextClockEvents)) |] : goPattern)
        (normalB $
         letE
         [ valD [p| (_, $(varP outState)) |] (normalB [| tick "" low $(varE state) $(makeMapping (Just currentClockEvents) portInputs) |]) []
         , valD [p| ($(varP clockTickOutMap), $(varP clockTickState)) |] (normalB [| foldl (\(_, s) (domId, edge) -> tick ($(varE clockNameMap) !! domId) (edgeToState edge) s Map.empty) (error "unused", $(varE outState)) $(varE currentClockEvents) |]) []
         ]
         [| ($(makeUnmapping clockTickOutMap outPortName), $(varE currentClockEvents)) :
            $(foldl appE [| $(varE cycleFunc) $(varE nextClockEvents) $(varE clockTickState) |] $ map (nextEventSignal currentClockEvents) portInputs )
          |]
        ) [] ]
    signalBody = letE
      [ initState
      , goInit
      , goCycle ]
      (foldl appE [| $(varE initFunc) $(varE initName)  |] $ map (varE . headAccessor) portInputs)

  pure $ ModuleTH
    { signal = [| Clash.Signal.fromList $ map fst $ filter ((($(lift $ portDomain output), Rising) `elem`) . snd) $ $signalBody |]
    }
  where
    -- | Signal to pass to the next clock event
    -- if a signal was consumed at this event, pass tail further
    -- otherwise leave the head
    -- TODO: use lazy pattern, to not force deconstruction and reconstruction here
    nextEventSignal :: Name -> PortTH -> ExpQ
    nextEventSignal currentEventName PortTH{portDomain, portDependency, headAccessor, tailAccessor} =
      [| if ($(lift portDomain), $(lift $ dependencyToEdge portDependency)) `elem` $(varE currentEventName) then
           $(varE tailAccessor)
         else
           $(varE headAccessor) :- $(varE tailAccessor)
       |]

    makeUnmapping :: Name -> String -> ExpQ
    makeUnmapping mapName portName = [| fromVectorBit (mapLookup $(varE mapName) $(liftString portName)) |]

    makeMapping :: Maybe Name -> [PortTH] -> ExpQ
    makeMapping currentEventName inputs =
      [| mapFromListMaybes
         $(listE $ map
            (\(PortTH{portName, portDomain, portDependency, headAccessor=a}) ->
                [| if $(maybe [| True |] (\eventName -> [| ($(lift portDomain), $(lift $ dependencyToEdge portDependency)) `elem` $(varE eventName) |]) currentEventName) then
                     Just ($(liftString portName), toVectorBit $(varE a))
                   else Nothing
                 |]) inputs) |]

    dependencyToEdge DependentPort    = Falling
    dependencyToEdge NotDependentPort = Rising

reorderTo :: [String] -> [(I.Port, a)] -> [(I.Port, a)]
reorderTo dest orig = map (orig List.!!) ind
  where
    origNames = map (I.pName . fst) orig
    ind = mapMaybe (`List.elemIndex` origNames) dest

reorderModsTo :: [String] -> [I.Module] -> [I.Module]
reorderModsTo dest orig = map (orig List.!!) ind
  where
    origOuts = map (I.pName . head . I.modOutputs) orig
    ind = mapMaybe (`List.elemIndex` origOuts) dest

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
convertOutputs dom outs = ty
  -- | Just dom <- dom = [t| Clash.Signal.Signal $(varT dom) $ty |]
  -- | otherwise = ty
  where
    -- Discard patterns and accessor names as they are not needed for the output
    ress = map (\(_, (name, _), c) -> c) $ convertPorts dom outs

    -- If there are multiple return values, construct a tuple
    ty = case ress of
      [] -> tupleT 0 -- unit type
      [single] -> single
      multiple -> foldl appT (tupleT (length multiple)) multiple
