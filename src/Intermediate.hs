{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Intermediate
  ( Direction(..)
  , Port(..)
  , Cell(..)
  , Net(..)
  , Module(..)
  , modMemorySize
  , ofJson
  , select
  )
where

import Data.Maybe (fromMaybe)

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List as L

import Data.Data (Data)

import qualified Json as J
import Memory

import Internal.Util

import qualified Intermediate.CellKinds as CellKinds

type Attribute = String

type AttrMap = Map String Attribute

data Direction = Input | Output | InOut
  deriving (Show, Eq, Data)

data Port = Port
  { pName :: String
  , pBits :: [BVE]
  } deriving Data

data Net = Net
  { nBits :: [BVE]
  , nAttributes :: AttrMap
  } deriving Data

data Cell = Cell
  { cKind :: CellKinds.Kind
  , cParameters :: Map String String
  , cInputs :: Map String [BVE]
  , cOutputs :: Map String [BVE]
  } deriving Data

data Module = Module
  { modName :: String
  , modAttributes :: AttrMap
  , modInputs :: [Port]
  , modOutputs :: [Port]
  , modCells :: [Cell]
  , modNetnames :: [Net]
  } deriving Data

modMemorySize :: Module -> Int
modMemorySize Module{modInputs=ins, modOutputs=outs, modNetnames=nets} = maximum' [pb ins, pb outs, nb nets]
  where
    pb = maximum' . map (maximum' . wires . pBits)
    nb = maximum' . map (maximum' . wires . nBits)
    
    maximum' [] = 0
    maximum' l@(_:_) = maximum l

ofJson :: J.TopLevel -> [Module]
ofJson (J.TopLevel mods) = map namedModule $ M.assocs mods
  where
    namedModule (name, J.Module {..}) =
      Module
        { modName = name,
          modAttributes = jsonModAttributes,
          modInputs = map namedPort ins,
          modOutputs = map namedPort outs,
          modCells = map (namedCell . snd) $ J.toList jsonModCells,
          -- modMemories = map namedMemo $ Map.assocs $ fromMaybe Map.empty memories
          modNetnames = map (namedNetName . snd) $ J.toList jsonModNetnames
        }
      where
        (ins, outs) = partitionPorts $ J.toList jsonModPorts

    partitionPorts :: [(String, J.Port)] -> ([(String, J.Port)], [(String, J.Port)])
    partitionPorts = go [] []
      where
        go ins outs [] = (ins, outs)
        go ins outs (port@(_, p) : rest) =
          case J.jsonPortDirection p of
            J.Input -> go (port : ins) outs rest
            J.Output -> go ins (port : outs) rest
            J.InOut -> go (port : ins) (port : outs) rest

    namedPort (name, J.Port {..}) =
      Port
        { pName = name,
          pBits = map bitWire jsonPortBits
        }

    namedCell J.Cell {..} =
      Cell
      { cKind = CellKinds.fromString jsonCellType
      , cParameters = jsonCellParameters
      , cInputs = ins
      , cOutputs = outs
      }
      where
        (ins, outs) = both (M.map (map bitWire) . M.fromList) . L.partition ((fromMaybe False . (== J.Input) <.> (`M.lookup` jsonCellPort_Directions)) . fst) $ M.toList jsonCellConnections

    namedNetName J.Net {..} =
      Net
        { nBits = map bitWire jsonNetBits,
          nAttributes = jsonNetAttributes
        }

    bitWire (J.Wire i) = W i
    bitWire (J.Bit c) = B (bit c)

select :: String -> [Module] -> Either String Module
select topLevelEntityName modules =
  case L.find ((== topLevelEntityName) . modName) modules of
    Just mod -> Right mod
    Nothing -> Left $ "no top-level entity named " ++ topLevelEntityName ++ " found"
