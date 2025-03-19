{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Internal.Intermediate
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

import Data.Text (Text)
import Data.Data (Data)

import qualified Internal.Json as J
import Internal.Memory

import qualified Internal.Intermediate.CellKinds as CellKinds

type Attribute = String

type AttrMap = Map String Attribute

data Direction = Input | Output | InOut
  deriving (Show, Eq, Data)

data Port = Port
  { pName :: String,
    pBits :: [BVE],
    pOffset :: Int,
    pSigned :: Bool
  } deriving Data

data Net = Net
  { nBits :: [BVE],
    nAttributes :: AttrMap,
    nSigned :: Bool
  } deriving Data

data Cell = Cell
  { cKind :: CellKinds.Kind,
    cAttributes :: AttrMap,
    cParameters :: Map String String,
    cPortDirections :: Map String Direction,
    cConnections :: Map String [BVE]
  } deriving Data

-- data Memory = Memory
--   { name :: String
--   , attributes :: AttrMap
--   , width :: Int
--   , start_offset :: Int
--   , size :: Int
--   }

data Module = Module
  { modName :: String,
    modAttributes :: AttrMap,
    modInputs :: [Port],
    modOutputs :: [Port],
    modCells :: [Cell],
    -- modMemories :: [Memory]
    modNetnames :: [Net]
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
          modCells = map namedCell $ J.toList jsonModCells,
          -- modMemories = map namedMemo $ Map.assocs $ fromMaybe Map.empty memories
          modNetnames = map namedNetName $ J.toList jsonModNetnames
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
          pBits = map bitWire jsonPortBits,
          pOffset = fromMaybe 0 jsonPortOffset,
          pSigned = Just 1 == jsonPortSigned
        }

    namedCell (name, J.Cell {..}) =
      Cell
      { cAttributes = jsonCellAttributes
      , cKind = CellKinds.fromString jsonCellType
      , cParameters = jsonCellParameters
      , cPortDirections = M.map (\case J.Input -> Input; J.Output -> Output; J.InOut -> InOut) jsonCellPort_Directions
      , cConnections = M.map (map bitWire) jsonCellConnections
      }

    namedNetName (name, J.Net {..}) =
      Net
        { nBits = map bitWire jsonNetBits,
          nAttributes = jsonNetAttributes,
          nSigned = Just 1 == jsonNetSigned
        }

    bitWire (J.Wire i) = W i
    bitWire (J.Bit c) = B (bit c)

select :: String -> [Module] -> Either String Module
select topLevelEntityName modules =
  case L.find ((== topLevelEntityName) . modName) modules of
    Just mod -> Right mod
    Nothing -> Left $ "no top-level entity named " ++ topLevelEntityName ++ " found"
