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

import Control.Arrow (first, (&&&), (***))
import Control.Monad (mapAndUnzipM)

import Data.Int (Int64)
import Data.Coerce (coerce)

import Data.Traversable (mapAccumM)
import Data.Maybe (fromJust, mapMaybe, catMaybes)
import qualified Data.Map as Map (Map, fromList, lookup, empty, (!))
import qualified Data.List as List
import Data.List.NonEmpty (unfoldr, NonEmpty((:|)))

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
      clocks = List.zip4
        [0,1..] -- clock index
        (reverse rawClocks) -- clock signals are prepended, reverse them, so the indices are correct
        (repeat (0 :: Int64)) -- current time of a clock, starting from 0
        (repeat Rising) -- current edge of the clock, starting from Rising edge at the beggining of time

      go clocks = map ($(accs 4 0) &&& $(accs 4 3)) currentEvents : go (map tickDom currentEvents ++ futureEvents)
        where
          currentTime = List.minimum $ map $(accs 4 2) clocks
          (currentEvents, futureEvents) = List.partition ((== currentTime) . $(accs 4 2)) clocks

          -- tick one clock period
          tickDom (ind, currentPeriod : nextPeriods, absT, edge) =
            (ind, nextPeriods, absT + currentPeriod, oppositeEdge edge)
          tickDom (_, [], _, _) = error "clock periods should never exhaust"

instance (ClockTick t, Clash.Signal.KnownDomain dom) => ClockTick (Clash.Signal.Clock dom -> t) where
  ticks' accumClocks clk
    | Just periods <- CLK.clockPeriods clk = ticks' (Clash.Signal.sample (unFemtoSeconds periods) : accumClocks)
    | SDomainConfiguration{sPeriod} <- knownDomain @dom = ticks' ((repeat . (* 1000) $ snatToNum sPeriod) : accumClocks)
    where
      domName = ssymbolToString $ CLK.clockTag clk
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
  :: ([String], [String])
  -- ^ Type annotation of a component with port names annotations
  -> FilePath
  -- ^ Path to the verilog design file
  -> ModuleOptions
  -- ^ Options
  -> Q [Dec]
externalComponent (argTyNames, retTyNames) filePath ModuleOptions{topEntityName=topLevelName, parameters=moduleParameters, ignoreCache=readFromCache, nonCombinatorialInputs=nonCombIns} = do
  topLevel <- runIO $ U.readDesign filePath (not readFromCache) topLevelName moduleParameters

  let markedInputs = markRolesForInputs topLevel
      markedOutputs = map (, Other) $ I.modOutputs topLevel
      maybeClockName = List.find ((== Clock) . snd) markedInputs
      specialSignalNames = map (I.pName . fst) $ filter ((== Clock) . snd) markedInputs
      markedDependentGroups = markDependentOutputs topLevel (specialSignalNames ++ nonCombIns)
      (nonDependent, dependentGroups) = both uniteDependentGroups $ List.partition ((== NotDependent) . snd) markedDependentGroups
      allDependentInputs = concatMap (dependencies . snd) dependentGroups
      argDoms = Map.fromList $ map (, 0 :: Int) argTyNames
      retDoms = Map.fromList $ map (, 0 :: Int) retTyNames

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

          inputNames = map (I.pName . fst) reorderedInputs
          (clockInputs, nonClockInputs) = both (map (first fst)) $ List.partition ((== Clock) . snd . fst) $ zip reorderedInputs acss
          -- `drop 1` is explained further
          otherInputs = filter (`notElem` allDependentInputs) $ map (I.pName . fst) nonClockInputs

          inputNameMap = Map.fromList $ zip inputNames (map fst acss)
      ticks <- newName "ticksCycle"
      currentTick <- newName "currentTick"
      ticksCont <- newName "ticksCont"
      signal <- newName "signal"
      state <- newName "state"
      goInit <- newName "goInit"
      goCycle <- newName "goCycle"
      stateND <- newName "stateND"
      initOutMap <- newName "initOutMap"
      domIdToClockName <- newName "clockNameMap"
      clockTickOutMap <- newName "clockTickOutMap"
      clockTickState <- newName "clockTickState"

      -- State and evaluation declarations are created in pairs
      -- One is needed for initialisation before the first clock cycle, therefore all the inputs should be evaluated unconditionally
      -- Another is used for cycling, when whether the input is set or not depends on clock events (which clock it is, rising/falling edge)
      -- There may be several clock events at a moment of time (i.e. two clocks where period of one is a multiple of a period of another)

      -- Set all inputs that cannot be combinatorial with outputs,
      -- setting all of them because non combinatorial dependency is not propagated
      -- If there is no non combinatorial inputs, it does nothing
      let nonDepInputsSet tickEvents =
            [d|
              (_, $(varP stateND)) =
                tick "" low $(varE state) $(makeMapping tickEvents (map (id &&& triple (argDoms Map.!) (const Rising) (inputNameMap Map.!)) otherInputs))
              |]
      nonDepInputsSet_Init <- nonDepInputsSet Nothing
      nonDepInputsSet_Cycle <- nonDepInputsSet (Just currentTick)

      -- the last state and evaluation declarations of outputs that do not depend on combinatorial inputs
      -- inputs for these outputs were set previously
      let nonDepStateEval tickEvent =
            case nonDependent of
              [nonDependent] -> do
                (outputMapName, func) <- evalGroup tickEvent []
                (map (, outputMapName) (fst nonDependent) ,) <$> func stateND
              [] -> pure ([], (stateND, []))
              _ -> error "should never have more than one non dependent group of outputs"

      -- here and next we do not care about map of where to take output from (the first element of a tuple)
      -- as output would be taken from the rising edge state that is created from the name of the state
      (_, (nonDepState_Init, nonDepEval_Init)) <- nonDepStateEval Nothing
      (_, (nonDepState_Cycle, nonDepEval_Cycle)) <- nonDepStateEval (Just currentTick)

      -- generates functions to make evaluations for dependent groups
      let depGroupsEval tickEvents = mapAndUnzipM
            (\(outs, dependencies -> deps) -> do
                (outputMapName, func) <- evalGroup tickEvents (map (id &&& triple (argDoms Map.!) (const Falling) (inputNameMap Map.!)) deps)
                pure (map (, outputMapName) outs, func)
            ) dependentGroups
      (_, depGroupsEval_Init) <- depGroupsEval Nothing
      (_, depGroupsEval_Cycle) <- depGroupsEval (Just currentTick)

      -- They also differ in the initial state here,
      -- declarations for the initialisation part takes non dependent evaluation part,
      -- as initialisation happens before the first clock cycle
      -- declaration for cycling takes state at the rising edge,
      -- by that point in time all the inputs should be stable and it should be safe to evaluate possibly combinatorial otputs
      (lastStateName_Init, dependentEval_Init) <- mapAccumM (\prevStateName func -> func prevStateName) nonDepState_Init depGroupsEval_Init
      (lastStateName_Cycle, dependentEval_Cycle) <- mapAccumM (\prevStateName func -> func prevStateName) nonDepState_Cycle depGroupsEval_Cycle

      -- `drop 1` in the declaration of `dec` drops the clock signal (which is the first after reordering)
      -- clock signal is not a real signal and is useless for the purpose of design evaluation
      dec <- funD name
        [clause inputPatterns
         (normalB $ tupE' $ zipWith
           (\i (port, _) ->
             [| Clash.Signal.fromList
                $ map ($(accs (length reorderedOutputs) i) . fst)
                $ filter ((($(lift $ retDoms Map.! I.pName port), Falling) `elem`) . snd)
                $ $(varE signal) |])
           [0,1..] reorderedOutputs)
           -- [| $(unzipFN (length reorderedOutputs)) $ $(varE signal) |] )
        -- where
          [ initD
          , valD (varP ticks) (normalB $ foldl appE [| clockTicks |] $ map (varE . fst . snd) clockInputs) []
          , valD (varP signal) (normalB $ foldl appE [| $(varE goInit) $(varE initStateName) |] $ map (varE . fst . snd) nonClockInputs) []
          -- Initialisation before the first clock cycle
          , funD goInit
            [ clause ((varP state) : map ((\(f, s) -> [p| ~($f :- $s) |]) . both varP) (drop 1 acss))
              (normalB $
               letE
                (concat
                  ( map pure nonDepInputsSet_Init
                  : map pure nonDepEval_Init
                  : [valD (varP initOutMap) (normalB [| peek $(varE nonDepState_Init) |] ) []]
                  : map (map pure) dependentEval_Init) )
                [| ($(makeUnmapping $ map ((, initOutMap) . I.pName . fst) reorderedOutputs), [(i, Falling) | i <- [0.. $(lift $ length clockInputs) - 1]]) :
                  ($(foldl appE [| $(varE goCycle) $(varE ticks) $(varE lastStateName_Init) |] $ map (varE . snd . snd) nonClockInputs))
                 |]
              ) [] ]
          , funD goCycle
            [ clause ([p| ($(varP currentTick) : $(varP ticksCont)) |] : (varP state) : map ((\(f, s) -> [p| ~($f :- $s) |]) . both varP) (drop 1 acss))
              (normalB $
                letE
                (concat $
                 ( map pure nonDepInputsSet_Cycle
                 : map pure nonDepEval_Cycle
                 : map (map pure) dependentEval_Cycle)
                 ++ [ [valD [p| ($(varP clockTickOutMap), $(varP clockTickState)) |]
                      (normalB [| foldl (\(_, s) (domId, edge) -> tick ($(varE domIdToClockName) !! domId) (edgeToState edge) s Map.empty) (error "unused", $(varE lastStateName_Cycle)) $(varE currentTick) |]) [] ] ] )
                [| ($(makeUnmapping $ map ((, clockTickOutMap) . I.pName . fst) reorderedOutputs), $(varE currentTick)) :
                  ($(foldl appE
                     [| $(varE goCycle) $(varE ticksCont) $(varE clockTickState) |]
                     $ map (portCont currentTick)
                     $ map (\(p, ns) -> ((argDoms Map.!) $ I.pName p, Falling, ns)) nonClockInputs))
                 |]
              ) [ valD (varP domIdToClockName) (normalB $ listE $ map (liftString . I.pName . fst) clockInputs) [] ] ]
          ] ]
      sig <- (sigD name [t| Clash.Signal.KnownDomain $(varT dom) => $(makeArrow argTys outTy) |])
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
         [ initD ] ]
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

    portCont :: Name -> (Int, Edge, (Name, Name)) -> ExpQ
    portCont currentEventName (portDomain, portComb, (h, t)) =
      [| if ($(lift portDomain), $(lift portComb)) `elem` $(varE currentEventName) then
          $(varE t)
         else ($(varE h) :- $(varE t))
       |]

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
  ([String], [String]) ->
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

unfoldType :: Type -> Q ([String], [String])
unfoldType (ForallT doms cxt arrow)
  | null cxt = unfoldType arrow
  | otherwise = error "expected no constraints"
unfoldType typ = do
  let (argTys, retTy) = unsnoc $ unfoldr (\case (AppT (AppT ArrowT arg) rest) -> (arg, Just rest); other -> (other, Nothing)) typ
  argTyNames <- arrowNames argTys
  pure (argTyNames, [])
  where
    -- annotated name, not the real name of the type
    typeName (AppT (AppT (ConT tripleColon) (LitT (StrTyLit name))) _)
      | nameBase tripleColon == ":::" = pure name
      | otherwise = error "something else but triple colon encountered"
    typeName _ = error "something completely wrong encountered"

    arrowNames :: [Type] -> Q [String]
    arrowNames = mapM typeName

    -- taken from cabal-syntax Distribution.Utils.Generic
    unsnoc :: NonEmpty a -> ([a], a)
    unsnoc (x :| xs) = go x xs
      where
        go y [] = ([], x)
        go y (z : zs) = let (ws, w) = go z zs in (y : ws, w)

reorderTo :: [String] -> [(I.Port, a)] -> [(I.Port, a)]
reorderTo dest orig = map (orig List.!!) ind
  where
    origNames = map (I.pName . fst) orig
    ind = mapMaybe (`List.elemIndex` origNames) dest

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
