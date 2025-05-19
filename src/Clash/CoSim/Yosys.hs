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
{-# LANGUAGE DataKinds #-}

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

import Control.Arrow ((&&&))
import Control.Monad (zipWithM)

import Data.Int (Int64)
import Data.Coerce (coerce)

import Data.Maybe (fromJust, mapMaybe, catMaybes)
import qualified Data.Map as Map
import qualified Data.List as List

import Data.Vector (Vector)
import qualified Data.Vector as V (singleton)

import Data.Function (on)

import Clash.Annotations.Primitive (PrimitiveGuard(HasBlackBox))
import Clash.Prelude (pack, ResetPolarity(ActiveHigh))
import Clash.Signal.Internal (Signal((:-)), Femtoseconds(..), SDomainConfiguration(..), knownDomain)
import qualified Clash.Signal.Internal as CLK (Clock(..))
import qualified Clash.Signal (KnownDomain, Signal, Clock, Reset, Enable, fromEnable, fromList, unsafeToActiveHigh, sample)
import qualified Clash.Sized.BitVector (BitVector)

import Clash.Promoted.Nat (snatToNum)

import qualified Intermediate as I
import qualified Memory as M
import qualified Interpreter as C
import qualified Util as U

import Clash.CoSim.Yosys.Internal
import Clash.CoSim.Yosys.Util
import Clash.CoSim.Yosys.Inst

-- import Debug.Trace

data Edge = Rising | Falling deriving (Eq, Show, Lift)

oppositeEdge :: Edge -> Edge
oppositeEdge Rising = Falling
oppositeEdge Falling = Rising

class ClockTick t where
  ticks' :: [[Int64]] -> t

instance ClockTick [[(Int, Edge)]] where
  ticks' rawClocks = go clockStates
    where
      clockStates = List.zip4
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

mapFromList :: Ord k => [(k, v)] -> Map.Map k v
mapFromList = Map.fromList

mapFromListMaybes :: Ord k => [Maybe (k, v)] -> Map.Map k v
mapFromListMaybes = Map.fromList . catMaybes

mapLookup :: Ord k => Map.Map k v -> k -> v
mapLookup = (Map.!)

maybeFromJust :: Maybe a -> a
maybeFromJust = fromJust
---

mkDomainName :: Int -> Name
mkDomainName n = mkName ("dom" ++ show n)

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
  -- :: ([(String, Int)], [(String, Int)])
  :: ([(String)], [(String)])
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
      (argTyNames, argDoms) = map fst &&& Map.fromList $ map (, 0) argTyNameDom
      (retTyNames, retDoms) = map fst &&& Map.fromList $ map (, 0) retTyNameDom
      specialSignalNames = map (I.pName . fst) $ filter ((== Clock) . snd) markedInputs
      mods = splitModule topLevel
      reorderedModules = reorderTo (I.pName . head . I.modOutputs) retTyNames mods

      reorderPorts = reorderTo (I.pName . fst)
      reorderedInputs = reorderPorts argTyNames markedInputs
      reorderedOutputs = reorderPorts retTyNames markedOutputs

  let name = mkName $ I.modName topLevel

  -- If there is something that resembles clock signal, then consider the design synchronous
  case maybeClockName of
    Just _ -> do
      inputPorts <- mapM (convertPort (Just argDoms)) reorderedInputs
      outputPorts <- mapM (convertPort (Just retDoms)) reorderedOutputs

      clockEvents <- newName "clockTicks"
      clockNames <- newName "clockNames"
      goInst <- newName "goInst"

      let
        portsMap = Map.fromList $ map (\port@PortTH{portName} -> (portName, port)) $ inputPorts ++ outputPorts
        clockInputPorts = List.sortBy (compare `on` portDomain) $ filter ((== Clock) . portRole) inputPorts
        instB = withInstNormalB [| $(mp (length outputPorts) 'unPort) $(varE goInst) |]

      dec <- funD name
        [ clause (map unwrapPattern inputPorts) (instB $ tupE' $ map (synchModuleE clockEvents clockNames portsMap) reorderedModules)
          [ valD (varP clockEvents) (normalB $ foldl appE [| clockTicks |] $ map (varE . headAccessor) clockInputPorts ) []
          , valD (varP clockNames) (normalB $ listE $ map (liftString . portName) clockInputPorts) []
          -- Synthesis
          , sigD goInst (tupT' $ map portTHtoInstPortT outputPorts)
          , valD (varP goInst) (normalB $ (foldl appE [| inst (instConfig $(liftString topLevelName)) |] $ map portTHtoInstPortE inputPorts)) []
          ]
        ]
      let doms = map mkDomainName $ List.nub $ Map.elems $ Map.union argDoms retDoms
      cxt <- mapM (\d -> [t| Clash.Signal.KnownDomain $(varT d) |]) doms
      sig <- sigD name $ forallT (map plainTV doms) (pure cxt) (makeArrow (map portSignal inputPorts) (tupT' $ map portSignal outputPorts))
      return [sig, dec]

    _ -> do
      inputPorts <- mapM (convertPort Nothing) reorderedInputs
      outputPorts <- mapM (convertPort Nothing) reorderedOutputs

      let portsMap = Map.fromList $ map (\port@PortTH{portName} -> (portName, port)) $ inputPorts ++ outputPorts
          instB = withInstNormalB [| error "Synthesis of asynchrnous designs is not supported" |]

      dec <- funD name [ clause (map unwrapPattern inputPorts) (instB $ tupE' $ map (asyncModuleE portsMap) reorderedModules) [] ]
      sig <- sigD name (makeArrow (map portType inputPorts) (tupT' $ map portType outputPorts))
      return [sig, dec]
  where

    tupT' [single] = single
    tupT' multi = foldl appT (tupleT (length multi)) multi

    tupE' [single] = single
    tupE' multi = tupE multi

    unTupE 1 = accs 1 0
    unTupE _ = [| id |]

    withInstNormalB inst body = guardedB
      [ (,) <$> normalG [| clashSimulation |] <*> body
      , (,) <$> normalG [| otherwise |] <*> inst
      ]

    makeArrow argTys retTy = foldr (appT . appT arrowT) retTy argTys

{- | Same as `externalComponent` but an expression, so inputs can be easily packed and unpacked for use

Continuing the second example from `externalComponent`,
```haskell
foo :: Unsigned 2 -> Bit -> Unsigned 3
foo a b = unpack $ $( externalComponentE "test.v" defaultOptions ) (pack a) (pack b)
```

-}
externalComponentE ::
  -- ([(String, Int)], [(String, Int)]) ->
  ([(String)], [(String)]) ->
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

dependencyToEdge :: PortDependency -> Edge
dependencyToEdge DependentPort    = Falling
dependencyToEdge NotDependentPort = Rising

data PortTH = PortTH
  { portName       :: String
  , portRole       :: Role
  , portDomain     :: Int
  , portType       :: Q Type
  , portDependency :: PortDependency
  , rawSignalName  :: Name
  , signalName     :: Name
  , headAccessor   :: Name
  , tailAccessor   :: Name
  }

portSignal :: PortTH -> Q Type
portSignal PortTH{portDomain, portType, portRole=Other} = [t| Clash.Signal.Signal $(varT $ mkDomainName portDomain) $portType |]
portSignal PortTH{portDomain, portType} = portType

unwrapPattern :: PortTH -> Q Pat
unwrapPattern PortTH{portRole=Clock, ..} = asP rawSignalName $ varP signalName
unwrapPattern PortTH{portRole=Reset, ..} = asP rawSignalName [p| (fmap pack . Clash.Signal.unsafeToActiveHigh -> $(varP signalName)) |]
unwrapPattern PortTH{portRole=Enable, ..} = asP rawSignalName [p| (fmap pack . Clash.Signal.fromEnable -> $(varP signalName)) |]
unwrapPattern PortTH{portRole=Other, ..} = asP rawSignalName $ varP signalName

destructivePattern :: PortTH -> Q Pat
destructivePattern PortTH{ signalName, headAccessor, tailAccessor} =
  asP signalName [p| ~($(varP headAccessor) :- $(varP tailAccessor)) |]

portTHtoInstPortE :: PortTH -> Q Exp
portTHtoInstPortE PortTH{..}
  | portRole == Clock = [| (ClockPort @($portNameS) $(varE rawSignalName)) |]
  | portRole == Reset = [| (ResetPort @($portNameS) @ActiveHigh $(varE rawSignalName)) |]
  | otherwise = [| (Port @($portNameS) $(varE signalName)) |]
  where
    portNameS :: Q Type
    portNameS = litT $ strTyLit portName

portTHtoInstPortT :: PortTH -> Q Type
portTHtoInstPortT PortTH{..}
  | portRole == Other = [t| (Port $(litT $ strTyLit portName) $(varT (mkDomainName portDomain)) $(portType)) |]
  | otherwise =
      -- Output port cannot have any other role but @Other@
      error "unexpected output port role"

synchModuleE :: Name -> Name -> Map.Map String PortTH -> I.Module -> ExpQ
synchModuleE clockEventsName clockNameMap portMap mod@I.Module{I.modInputs=inputs, I.modOutputs=outputs} = do
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
          [ valD [p| ($(varP outMap), $(varP outState)) |] (normalB [| tick "" low $(varE state) $(makeMapping Nothing portInputs) |]) []
          -- , valD [p| (_, $(varP outState)) |] (normalB [| tick "" low $(varE tempState) $(makeMapping Nothing $ filter ((== DependentPort) . portDependency) portInputs) |]) []
          ]
          -- Before first clock cycle is the same for all clock signals,
          -- I don't want to check how many clock signals there are, so just generate an infinite list with all possible
          [| ($(makeUnmapping outMap outPortName), [(i, Falling) | i <- [0,1..]]) :
          -- [| traceShow ($(makeMapping Nothing portInputs)) $ ($(makeUnmapping outMap outPortName), [(i, Rising) | i <- [0,1..]]) :
             $(foldl appE [| $(varE cycleFunc) $(varE clockEventsName) $(varE outState) |] $ map (varE . tailAccessor) portInputs)
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
         -- [| traceShow ($(varE currentClockEvents), $(makeMapping Nothing portInputs)) $ ($(makeUnmapping clockTickOutMap outPortName), $(varE currentClockEvents)) :
            $(foldl appE [| $(varE cycleFunc) $(varE nextClockEvents) $(varE clockTickState) |] $ map (nextEventSignal currentClockEvents) portInputs )
          |]
        ) [] ]
    signalBody = letE
      [ initState
      , goInit
      , goCycle ]
      (foldl appE [| $(varE initFunc) $(varE initName)  |] $ map (varE . signalName) portInputs)

  [| Clash.Signal.fromList $ map fst $ filter ((($(lift $ portDomain output), Falling) `elem`) . snd) $ $signalBody |]
  where
    -- | Signal to pass to the next clock event
    -- if a signal was consumed at this event, pass tail further
    -- otherwise leave the signal unchanged
    nextEventSignal :: Name -> PortTH -> ExpQ
    nextEventSignal currentEventName PortTH{portDomain, portDependency, tailAccessor, signalName} =
      [| if ($(lift portDomain), $(lift $ dependencyToEdge portDependency)) `elem` $(varE currentEventName) then
           $(varE tailAccessor)
         else
           $(varE signalName)
       |]

asyncModuleE :: Map.Map String PortTH -> I.Module -> ExpQ
asyncModuleE portMap mod@I.Module{I.modInputs=inputs, I.modOutputs=outputs} = do
  output <- case outputs of
    [output] -> pure (portMap Map.! I.pName output)
    _ -> error "separate modules expected to have only one output"

  let part = I.modName mod ++ "_part"
  let portInputs = map ((portMap Map.!) . I.pName) inputs

  initName <- newName part
  outputMapName <- newName "output"

  letE
    [ valD [p| ($(varP outputMapName), _) |] (normalB [| tick "" low $(varE initName) $(makeMapping Nothing portInputs) |]) []
    , valD (varP initName) (normalB [| compile $(liftData mod) |]) []
    -- , valD (varP initName) (normalB [| undefined |]) []
    ]
    (makeUnmapping outputMapName (portName output))

makeMapping :: Maybe Name -> [PortTH] -> ExpQ
makeMapping currentEventName inputs =
  [| mapFromListMaybes
     $(listE $ map
        (\(PortTH{portName, portDomain, portDependency, headAccessor}) ->
            [| if $(maybe [| True |] (\eventName -> [| ($(lift portDomain), $(lift $ dependencyToEdge portDependency)) `elem` $(varE eventName) |]) currentEventName) then
                 Just ($(liftString portName), toVectorBit $(varE headAccessor))
               else Nothing
             |]) inputs) |]

makeUnmapping :: Name -> String -> ExpQ
makeUnmapping mapName portName = [| fromVectorBit (mapLookup $(varE mapName) $(liftString portName)) |]

reorderTo :: Eq a => (b -> a) -> [a] -> [b] -> [b]
reorderTo f dst orig = map (orig List.!!) ind
  where
    origInd = map f orig
    ind = mapMaybe (`List.elemIndex` origInd) dst

convertPort ::
  Maybe (Map.Map String Int) ->
  -- ^ Map of domain indexes, Nothing means no mapping, no clock, asynchronous design
  (I.Port, Role) ->
  -- ^ Port with its attached role
  Q PortTH
  -- ^ Record with all later usefull information

convertPort (Just domainMap) (I.Port{..}, Clock)
  | Just d <- Map.lookup pName domainMap = do
      rawName <- newName pName
      signalName <- newName pName
      pure $ PortTH pName Clock d [t| Clash.Signal.Clock $(varT $ mkDomainName d) |] NotDependentPort rawName signalName signalName signalName
  | otherwise = error $ "No domain index was given to the clock port \"" ++ pName ++ "\""
convertPort (Just domainMap) (I.Port{..}, Reset)
  | Just d <- Map.lookup pName domainMap = do
      rawName <- newName pName
      signalName <- newName pName
      headName <- newName (pName ++ "_head")
      tailName <- newName (pName ++ "_tail")
      pure $ PortTH pName Reset d [t| Clash.Signal.Reset $(varT $ mkDomainName d) |] NotDependentPort rawName signalName headName tailName
  | otherwise = error $ "No domain index was given to the reset port \"" ++ pName ++ "\""
convertPort (Just domainMap) (I.Port{..}, Enable)
  | Just d <- Map.lookup pName domainMap = do
      rawName <- newName pName
      signalName <- newName pName
      headName <- newName (pName ++ "_head")
      tailName <- newName (pName ++ "_tail")
      pure $ PortTH pName Enable d [t| Clash.Signal.Enable $(varT $ mkDomainName d) |] NotDependentPort rawName signalName headName tailName
  | otherwise = error $ "No domain index was given to the enable port \"" ++ pName ++ "\""
convertPort domainMap ((I.Port pName (length -> bitSize)), _)
  | Just domainMap' <- domainMap
  , Just d <- Map.lookup pName domainMap' = do
      rawName <- newName pName
      signalName <- newName pName
      headName <- newName (pName ++ "_head")
      tailName <- newName (pName ++ "_tail")
      pure $ PortTH pName Other d bv2type DependentPort rawName signalName headName tailName
  | Just domainMap' <- domainMap = error $ "No domain index was given to the port \"" ++ pName ++ "\""
  | otherwise = do
      rawName <- newName pName
      signalName <- newName pName
      pure $ PortTH pName Other 0 bv2type DependentPort rawName signalName signalName signalName
  where
    bv2type = [t| Clash.Sized.BitVector.BitVector $(fromIntegral bitSize) |]
