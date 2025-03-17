{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.CoSim.Yosys
  ( externalComponent
  , externalComponentE
  , tick
  -- * Functions to make expanded splice happy
  , mapFromList
  , mapLookup
  , PrimitiveGuard(HasBlackBox)
  )
where

import Language.Haskell.TH hiding (Role)
import Language.Haskell.TH.Syntax (liftString, liftData)

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map (Map, fromList, lookup, (!))
import qualified Data.List as List (nub, partition, find)
import Data.Either.Extra (fromRight')

import Clash.Prelude (pack)
import Clash.Signal.Internal (Signal((:-)))
import qualified Clash.Signal (KnownDomain, Signal, Clock, Reset, Enable, fromEnable, unsafeToActiveHigh)
import qualified Clash.Sized.BitVector (BitVector)
import Clash.Annotations.Primitive (PrimitiveGuard(HasBlackBox))

import Interp.Main (readDesign)
import qualified MyLib as I
import qualified Memory as M
import qualified Graph as G

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

tick :: String -> G.Design -> Map.Map String Integer -> (Map.Map String (Maybe Integer), G.Design)
tick clockName state ins =
  let
    state' = G.eval $ put (Map.fromList [(clockName, 1)]) $ G.eval $ put ins $ G.eval $ put (Map.fromList [(clockName, 0)]) state
    out = G.peek state'
  in
    (out, G.eval $ put (Map.fromList [(clockName, 0)]) state')
  where
    put = flip G.start

compile :: NonEmpty I.Module -> G.Design
compile mods = fromRight' $ G.compile mods

mapFromList :: Ord k => [(k, v)] -> Map.Map k v
mapFromList = Map.fromList

mapLookup :: Ord k => Map.Map k v -> k -> v
mapLookup = (Map.!)

externalComponent :: FilePath -> Q [Dec]
externalComponent filePath = do
  modules@(topLevel :| _) <- runIO $ readDesign filePath
  let name = mkName $ I.modName topLevel
      markedInputs = markInputs topLevel
      markedOutputs = map (, Other) $ I.modOutputs topLevel
      clockName = List.find ((== Clock) . snd) markedInputs
      init = mkName "initState"
      initD = valD (varP init) (normalB [| compile $(liftData modules) |]) []
  noinlineAnn <- pragInlD name NoInline FunLike AllPhases
  hasBlackBoxAnn <- pragAnnD (ValueAnnotation name) [| (HasBlackBox [] ()) |]
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
      dec <- funD name
        [clause names (normalB (foldl appE [| $(varE go) $(varE init) |] $ drop 1 $ map (varE . fst) acss) )
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
      dec <- funD name
        [clause names (normalB [| 0 |]) []]
      sig <- sigD name (makeArrow argTys outTy)
      return [sig, dec, noinlineAnn, hasBlackBoxAnn]
  where
    makeMapping inputs = [| mapFromList $(listE $ map (\(p, a) -> [| ($(liftString $ I.pName p), toInteger $a) |]) inputs) |]
    makeUnmapping out [output] = makeSingleUnmap out output
    makeUnmapping out outputs = tupE $ map (makeSingleUnmap out) outputs
    makeSingleUnmap out output = [| fromInteger $ fromJust (mapLookup $out $(liftString $ I.pName output)) |]
    makeArrow argTys retTy = foldr (appT . appT arrowT) retTy argTys

externalComponentE :: FilePath -> Q Exp
externalComponentE filePath = do
  -- It may not be the best thing to do, but it's precisely known what `externalComponent` is going to return
  decs@(SigD name _ : _) <- externalComponent filePath
  letE (map pure decs) (varE name)

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
    clock = [(== "clk"), (== "clock")]
    reset = [(== "rst"), (== "arst"), (== "reset")]
    enable = [(== "en"), (== "enable")]

    mark p@I.Port{I.pName, I.pBits}
      | like clock pName = (p, Clock)
      | like reset pName = (p, Reset)
      | like enable pName = (p, Enable)
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

convertPort :: Maybe Name -> (I.Port, Role) -> (Q Pat, (Name, Name), Q Type)
convertPort (Just d) (I.Port{..}, Clock) = (pat, (acs, acs), [t| Clash.Signal.Clock $(varT d) |])
  where pat = varP n
        acs = n
        n = mkName pName
convertPort (Just d) (I.Port{..}, Reset) =
  ([p| (fmap pack . Clash.Signal.unsafeToActiveHigh -> $(varP rstName)) |],
   (rstName, rstSig),
   [t| Clash.Signal.Reset $(varT d) |])
  where rstName = mkName pName
        rstSig = mkName (pName ++ "sig")
convertPort (Just d) (I.Port{..}, Enable) =
  ([p| (fmap pack . Clash.Signal.fromEnable -> $(varP enName)) |],
    (enName, enSig),
    [t| Clash.Signal.Enable $(varT d) |])
  where enName = mkName pName
        enSig = mkName (pName ++ "sig")
convertPort dom (I.Port{I.pName=portName, I.pBits=bits}, _)
  | Just d <- dom = (varP sigHead, (sigHead, sigTail), bv2signal d)
  | otherwise = (varP sigHead, (sigHead, sigHead), bv2type)
  where
    sigHead = mkName portName
    sigTail = mkName (portName ++ "_tail")

    bv2type = [t| Clash.Sized.BitVector.BitVector $(fromIntegral $ length bits) |]
    bv2signal d = [t| Clash.Signal.Signal $(varT d) $bv2type |]

convertMapping :: Maybe Name -> [(I.Port, Role)] -> [(Q Pat, (Name, Name), Q Type)]
convertMapping dom = map (convertPort dom)

convertInputs :: Maybe Name -> [(I.Port, Role)] -> ([Q Pat], [(Name, Name)], [Q Type])
convertInputs dom ins = (names, acss, args)
  where
    (names, acss, args) = unzip3 $ convertMapping dom ins
    -- pat = map (\n -> (n, mkName n)) names

convertOutputs :: Maybe Name -> [(I.Port, Role)] -> Q Type
convertOutputs dom outs = ty
  where
    ress = map (\(_, _, c) -> c) $ convertMapping dom outs

    ty = case ress of
      [single] -> single
      multiple -> foldl appT (tupleT (length multiple)) multiple
