{-# LANGUAGE TemplateHaskell #-}

module Arkham.Direction (
  module Arkham.Direction,
) where

import Arkham.Prelude

import Data.Aeson.TH

data Direction = Above | Below | LeftOf | RightOf
  deriving stock (Show, Eq, Ord, Enum, Bounded, Data)

data GridDirection = GridUp | GridDown | GridLeft | GridRight
  deriving stock (Show, Eq, Ord, Enum, Bounded, Data)

pattern North :: GridDirection
pattern North <- GridUp
  where
    North = GridUp

pattern South :: GridDirection
pattern South <- GridDown
  where
    South = GridDown

pattern East :: GridDirection
pattern East <- GridRight
  where
    East = GridRight

pattern West :: GridDirection
pattern West <- GridLeft
  where
    West = GridLeft

oppositeDirection :: GridDirection -> GridDirection
oppositeDirection = \case
  GridUp -> GridDown
  GridDown -> GridUp
  GridLeft -> GridRight
  GridRight -> GridLeft

$(deriveJSON defaultOptions ''Direction)
$(deriveJSON defaultOptions ''GridDirection)

instance ToJSONKey Direction
instance FromJSONKey Direction
