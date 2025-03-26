{-# LANGUAGE DeriveDataTypeable #-}

module Intermediate.CellKinds where

import Data.Data (Data)
import Data.Typeable (Typeable)

data Kind
  = Buf
  | LogicNot
  | Neg
  | Not
  | Pos
  | ReduceAnd
  | ReduceBool
  | ReduceOr
  | ReduceXnor
  | ReduceXor
  | Add
  | And
  | Mul
  | Sub
  | Bweqx
  | Div
  | Divfloor
  | Mod
  | Modfloor
  | Eq
  | Eqx
  | Ge
  | Gt
  | Le
  | Lt
  | LogicAnd
  | LogicOr
  | Or
  | Xor
  | Bmux
  | Bwmux
  | Demux
  | Mux
  | Pmux
  | Tribuf
  | Adff
  | Adffe
  | Adlatch
  | Aldff
  | Aldffe
  | Dff
  | Dffe
  | Sdff
  | SubDesign String
  deriving (Data, Typeable)

fromString :: String -> Kind
fromString "$buf" = Buf
fromString "$logic_not" = LogicNot
fromString "$neg" = Neg
fromString "$not" = Not
fromString "$pos" = Pos
fromString "$reduce_and" = ReduceAnd
fromString "$reduce_bool" = ReduceBool
fromString "$reduce_or" = ReduceOr
fromString "$reduce_xnor" = ReduceXnor
fromString "$reduce_xor" = ReduceXor
fromString "$add" = Add
fromString "$and" = And
fromString "$mul" = Mul
fromString "$sub" = Sub
fromString "$bweqx" = Bweqx
fromString "$div" = Div
fromString "$divfloor" = Divfloor
fromString "$mod" = Mod
fromString "$modfloor" = Modfloor
fromString "$eq" = Eq
fromString "$eqx" = Eqx
fromString "$ge" = Ge
fromString "$gt" = Gt
fromString "$le" = Le
fromString "$lt" = Lt
fromString "$logic_and" = LogicAnd
fromString "$logic_or" = LogicOr
fromString "$or" = Or
fromString "$xor" = Xor
fromString "$bmux" = Bmux
fromString "$bwmux" = Bwmux
fromString "$demux" = Demux
fromString "$mux" = Mux
fromString "$pmux" = Pmux
fromString "$tribuf" = Tribuf
fromString "$adff" = Adff
fromString "$adffe" = Adffe
fromString "$adlatch" = Adlatch
fromString "$aldff" = Aldff
fromString "$aldffe" = Aldffe
fromString "$dff" = Dff
fromString "$dffe" = Dffe
fromString "$sdff" = Sdff
fromString other = SubDesign other

hasClock :: Kind -> Bool
hasClock Adff = True
hasClock Adffe = True
hasClock Aldff = True
hasClock Aldffe = True
hasClock Dff = True
hasClock Dffe = True
hasClock Sdff = True
hasClock _ = False
