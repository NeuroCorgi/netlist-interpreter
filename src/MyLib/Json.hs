
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE LambdaCase            #-}

module MyLib.Json
  ( TopLevel(..)
  , Module(..)
  , Port(..)
  , Net(..)
  , Cell(..)
  , Memory(..)
  , Direction(..)
  , Q(..)
  , parseModule
  )
where

import Data.Text ( Text, unpack )
import qualified Data.Text.Encoding as E

import Data.Map ( Map )

import Data.ByteString.Lazy ( fromStrict )

import Data.Aeson.TH ( deriveFromJSON, defaultOptions, Options(..) )
import Data.Aeson ( eitherDecode, parseJSON, FromJSON, Value(String, Number), )

import Data.Scientific (toBoundedInteger)

import Control.Applicative (empty)

type Attribute = String
type AttrMap = Map String Attribute

data Direction = Input | Output | InOut

instance FromJSON Direction where
  parseJSON (String dir) =
    case dir of
      "input" -> return Input
      "output" -> return Output
      "inout" -> return InOut
      _ -> empty

  parseJSON _ = empty

data Port = Port
  { direction :: Direction
  , bits :: [Int]
  , offset :: Maybe Int
  , upto :: Maybe Bool
  , signed :: Maybe Bool
  }
$( deriveFromJSON defaultOptions 'Port )

data Net = Net
  { bits :: [Int]
  , offset :: Maybe Int
  , attributes :: AttrMap
  , upto :: Maybe Bool
  , signed :: Maybe Bool
  }
$( deriveFromJSON defaultOptions 'Net )

data Q = Wire Int | Bit Char

instance FromJSON Q where
  parseJSON (String b) =
    case unpack b of
      [b'] -> return (Bit b')
      _ -> empty
  parseJSON (Number n) =
    case toBoundedInteger n of
      Just n' -> return (Wire n')
      Nothing -> empty
  parseJSON _ = empty

data Cell = Cell
  { typ             :: String
  , attributes      :: AttrMap
  , parameters      :: Map String String
  , port_directions :: Map String Direction
  , connections     :: Map String [Q]
  }
$( deriveFromJSON defaultOptions{fieldLabelModifier= \case "typ" -> "type"; a -> a} 'Cell )

data Memory = Memory
  { attributes :: AttrMap
  , width :: Int
  , start_offset :: Int
  , size :: Int
  }
$( deriveFromJSON defaultOptions 'Memory )


data Module = Module
  { attributes :: AttrMap
  , ports    :: Maybe (Map String Port)
  , cells    :: Maybe (Map String Cell)
  , memories :: Maybe (Map String Memory)
  , netnames :: Maybe (Map String Net)
  }

$( deriveFromJSON defaultOptions{rejectUnknownFields=True} 'Module )

newtype TopLevel = TopLevel { modules :: Map String Module }
$( deriveFromJSON defaultOptions{rejectUnknownFields=False} 'TopLevel )

parseModule :: Text -> Either String TopLevel
parseModule = eitherDecode . fromStrict . E.encodeUtf8
