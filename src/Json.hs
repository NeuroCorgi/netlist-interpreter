{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Json
  ( TopLevel(..)
  , Module(..)
  , Port(..)
  , Net(..)
  , Cell(..)
  , Direction(..)
  , Q(..)
  , Assoc(..)
  , parseModule
  )
where

import Control.Applicative (empty)

import Data.Aeson ( eitherDecode, parseJSON, FromJSON, Value(Object, String, Number), )
import Data.Aeson.TH ( deriveFromJSON, defaultOptions, Options(..) )
import qualified Data.Aeson.KeyMap as KM ( toList )
import qualified Data.Aeson.Key as K ( toString )

import Data.ByteString.Lazy ( fromStrict )
import Data.Char ( toLower )
import Data.Text ( Text, unpack )
import Data.Map ( Map )

import qualified Data.Text.Encoding as E

import Data.Scientific (toBoundedInteger)

type Attribute = String
type AttrMap = Map String Attribute

newtype Assoc a = Assoc { toList :: [(String, a)] }

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

data Direction = Input | Output | InOut deriving Eq

instance FromJSON Direction where
  parseJSON (String dir) =
    case dir of
      "input" -> return Input
      "output" -> return Output
      "inout" -> return InOut
      _ -> empty

  parseJSON _ = empty

instance FromJSON a => FromJSON (Assoc a) where
  parseJSON (Object obj) =
    Assoc <$> mapM (\(k, v) -> (K.toString k, ) <$> parseJSON v) (KM.toList obj)
  parseJSON _ = empty

data Port = Port
  { jsonPortDirection :: Direction
  , jsonPortBits      :: [Q]
  -- Those ports are not used, and, therefore, not needed
  -- jsonPortOffset    :: Maybe Int
  -- There is also `upto` field, but it is useless as bits are already correctly arranged
  -- jsonPortSigned    :: Maybe Int
  }
$( deriveFromJSON defaultOptions{ fieldLabelModifier=map toLower . drop 8 } 'Port )

data Net = Net
  { jsonNetBits       :: [Q]
  , jsonNetAttributes :: AttrMap
  -- jsonNetOffset     :: Maybe Int
  -- upto
  -- jsonNetSigned     :: Maybe Int
  }
$( deriveFromJSON defaultOptions{ fieldLabelModifier=map toLower . drop 7 } 'Net )

data Cell = Cell
  { jsonCellType           :: String
  , jsonCellAttributes     :: AttrMap
  , jsonCellParameters     :: Map String String
  , jsonCellPort_Directions :: Map String Direction
  , jsonCellConnections    :: Map String [Q]
  }
$( deriveFromJSON defaultOptions{ fieldLabelModifier=map toLower . drop 8 } 'Cell )

data Module = Module
  { jsonModAttributes :: AttrMap
  , jsonModPorts      :: Assoc Port
  , jsonModCells      :: Assoc Cell
  -- Memory is mapped into registers by one of the yosys passes
  , jsonModNetnames   :: Assoc Net
  }

$( deriveFromJSON defaultOptions{ fieldLabelModifier=map toLower . drop 7 } 'Module )

newtype TopLevel = TopLevel { modules :: Map String Module }
$( deriveFromJSON defaultOptions{rejectUnknownFields=False} 'TopLevel )

parseModule :: Text -> Either String TopLevel
parseModule = eitherDecode . fromStrict . E.encodeUtf8
