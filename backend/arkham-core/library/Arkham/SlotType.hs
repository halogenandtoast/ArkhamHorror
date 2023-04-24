{-# LANGUAGE TemplateHaskell #-}
module Arkham.SlotType where

import Arkham.Prelude

import Data.Aeson.TH

data SlotType
  = HandSlot
  | BodySlot
  | AllySlot
  | AccessorySlot
  | ArcaneSlot
  | TarotSlot
  deriving stock (Show, Ord, Eq)

$(deriveJSON defaultOptions ''SlotType)

instance ToJSONKey SlotType
instance FromJSONKey SlotType
