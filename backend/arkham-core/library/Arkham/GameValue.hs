{-# LANGUAGE TemplateHaskell #-}
module Arkham.GameValue
  ( GameValue(..)
  , fromGameValue
  , toGameValue
  ) where

import Arkham.Prelude

import Data.Aeson.TH

data GameValue
  = Static Int
  | PerPlayer Int
  | StaticWithPerPlayer Int Int
  | ByPlayerCount Int Int Int Int
  deriving stock (Show, Eq, Ord)

class IsGameValue a where
  toGameValue :: a -> GameValue

instance IsGameValue GameValue where
  toGameValue = id

instance IsGameValue Int where
  toGameValue = Static

fromGameValue :: GameValue -> Int -> Int
fromGameValue (Static n) _ = n
fromGameValue (PerPlayer n) pc = n * pc
fromGameValue (StaticWithPerPlayer n m) pc = n + (m * pc)
fromGameValue (ByPlayerCount n1 n2 n3 n4) pc = case pc of
  1 -> n1
  2 -> n2
  3 -> n3
  4 -> n4
  _ -> error "Unhandled by player count value"

$(deriveJSON defaultOptions ''GameValue)
