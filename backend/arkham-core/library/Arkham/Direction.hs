{-# LANGUAGE TemplateHaskell #-}

module Arkham.Direction (
  module Arkham.Direction,
) where

import Arkham.Prelude

import Data.Aeson.TH

data Direction = Above | Below | LeftOf | RightOf
  deriving stock (Show, Eq, Ord, Enum, Bounded)

data GridDirection = GridUp | GridDown | GridLeft | GridRight
  deriving stock (Show, Eq, Ord, Enum, Bounded)

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
