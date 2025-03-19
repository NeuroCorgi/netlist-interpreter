{-# LANGUAGE DeriveDataTypeable #-}

module Internal.Memory.Bit
  ( Bit(..)
  , fromBool
  , complement
  , (.&.)
  , (.|.)
  , xor
  , implies
  , equal
  , equal'
  )
where

import Data.Data (Data)
import Data.Typeable (Typeable)

data Bit
  = H
  | L
  | X
  | Z
  deriving (Eq, Show, Data, Typeable)

fromBool :: Bool -> Bit
fromBool True = H
fromBool False = L

complement :: Bit -> Bit
complement L = H
complement H = L
complement _ = X

infixl 7 .&.
(.&.) :: Bit -> Bit -> Bit
L .&. _ = L
_ .&. L = L
X .&. _ = X
_ .&. X = X
Z .&. _ = X
_ .&. Z = X
H .&. H = H

infixl 5 .|.
(.|.) :: Bit -> Bit -> Bit
H .|. _ = H
_ .|. H = H
X .|. _ = X
_ .|. X = X
Z .|. _ = X
_ .|. Z = X
L .|. L = L

infixl 6 `xor`
xor :: Bit -> Bit -> Bit
xor a b = (a .&. nb) .|. (na .&. b)
  where
    na = complement a
    nb = complement b

implies :: Bit -> Bit -> Bit
implies a b = (complement a) .|. b

equal :: Bit -> Bit -> Bit
equal a b = implies a b .&. implies b a

equal' :: Bit -> Bit -> Bool
equal' L L = True
equal' H H = True
equal' _ _ = False
