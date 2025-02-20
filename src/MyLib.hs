{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module MyLib where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified MyLib.Json as J

import Memory

type Attribute = String

type AttrMap = Map String Attribute

data Direction = Input | Output | InOut
  deriving (Show, Eq)

data Port = Port
  { pName :: String,
    direction :: Direction,
    pBits :: [Int],
    offset :: Int,
    upto :: Bool,
    signed :: Bool
  }
  deriving (Show)

data Net = Net
  { name :: String,
    nBits :: [Int],
    offset :: Int,
    upto :: Bool,
    signed :: Bool
  }
  deriving (Show)

data Cell = Cell
  { name :: String,
    typ :: String,
    attributes :: AttrMap,
    parameters :: Map String String,
    port_directions :: Map String Direction,
    connections :: Map String [BVE]
  }
  deriving (Show)

-- data Memory = Memory
--   { name :: String
--   , attributes :: AttrMap
--   , width :: Int
--   , start_offset :: Int
--   , size :: Int
--   }
--   deriving Show

data Module = Module
  { name :: String,
    attributes :: AttrMap,
    ports :: [Port],
    cells :: [Cell],
    -- , memories :: [Memory]
    netnames :: [Net]
  }
  deriving Show

ofJson :: J.TopLevel -> [Module]
ofJson (J.TopLevel mods) = map namedModule $ Map.assocs mods
  where
    namedModule (name, J.Module {..}) =
      Module
        { name = name,
          attributes = attributes,
          ports = map namedPort $ Map.assocs $ fromMaybe Map.empty ports,
          cells = map namedCell $ Map.assocs $ fromMaybe Map.empty cells,
          -- , memories=map namedMemo $ Map.assocs $ fromMaybe Map.empty memories
          netnames = map namedNetName $ Map.assocs $ fromMaybe Map.empty netnames
        }

    namedPort (name, J.Port {..}) =
      Port
        { pName = name,
          direction = (\case J.Input -> Input; J.Output -> Output; J.InOut -> InOut) direction,
          pBits = bits,
          offset = fromMaybe 0 offset,
          upto = fromMaybe False upto,
          signed = fromMaybe False signed
        }

    bitWire (J.Wire i) = W i
    bitWire (J.Bit c) = B (bit c)
    namedCell (name, J.Cell {..}) =
      Cell
        { name = name,
          attributes = attributes,
          typ = typ,
          parameters = parameters,
          port_directions = Map.map (\case J.Input -> Input; J.Output -> Output; J.InOut -> InOut) port_directions,
          connections = Map.map (map bitWire) connections
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
        { name = name,
          nBits = bits,
          offset = fromMaybe 0 offset,
          upto = fromMaybe False upto,
          signed = fromMaybe False signed
        }

mods :: Text -> Either String [Module]
mods i = ofJson <$> J.parseModule i
