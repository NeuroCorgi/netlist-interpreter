{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module MyLib where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified MyLib.Json as J

import Data.Data (Data)
import Data.Typeable (Typeable)

import Memory
import CellKinds

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

ofJson :: J.TopLevel -> [Module]
ofJson (J.TopLevel mods) = map namedModule $ Map.assocs mods
  where
    namedModule (name, J.Module {..}) =
      Module
        { modName = name,
          modAttributes = attributes,
          modInputs = map namedPort ins,
          modOutputs = map namedPort outs,
          modCells = map namedCell $ Map.assocs $ fromMaybe Map.empty cells,
          -- modMemories = map namedMemo $ Map.assocs $ fromMaybe Map.empty memories
          modNetnames = map namedNetName $ Map.assocs $ fromMaybe Map.empty netnames
        }
      where
        (ins, outs) = partitionPorts $ Map.assocs $ fromMaybe Map.empty ports
        
    partitionPorts :: [(String, J.Port)] -> ([(String, J.Port)], [(String, J.Port)])
    partitionPorts = go [] []
      where
        go ins outs [] = (ins, outs)
        go ins outs (port@(_, p) : rest) =
          case J.direction p of
            J.Input -> go (port : ins) outs rest
            J.Output -> go ins (port : outs) rest
            J.InOut -> go (port : ins) (port : outs) rest

    namedPort (name, J.Port {..}) =
      Port
        { pName = name,
          pBits = map bitWire bits,
          pOffset = fromMaybe 0 offset,
          pSigned = Just 1 == signed
        }

    namedCell (name, J.Cell {..}) =
      Cell
      { cAttributes = attributes
      , cKind = CellKinds.fromString typ
      , cParameters = parameters
      , cPortDirections = Map.map (\case J.Input -> Input; J.Output -> Output; J.InOut -> InOut) port_directions
      , cConnections = Map.map (map bitWire) connections
      }

    -- namedMemo (name, J.Memory{..}) =
    --   Memory
    --   { name=name
    --   , attributes=attributes
    --   , width=width
    --   , start_offset=start_offset
    --   , size=size
    --   }

    namedNetName (name, J.Net {..}) =
      Net
        { nBits = map bitWire bits,
          nAttributes = attributes,
          nSigned = Just 1 == signed
        }

    bitWire (J.Wire i) = W i
    bitWire (J.Bit c) = B (bit c)

mods :: Text -> Either String [Module]
mods i = ofJson <$> J.parseModule i
