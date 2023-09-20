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
  deriving stock (Show, Ord, Eq, Bounded, Enum, Data)

slotName :: SlotType -> Text
slotName = \case
  HandSlot -> "Hand"
  BodySlot -> "Body"
  AllySlot -> "Ally"
  AccessorySlot -> "Accessory"
  ArcaneSlot -> "Arcane"
  TarotSlot -> "Tarot"

allSlotTypes :: [SlotType]
allSlotTypes = [minBound .. maxBound]

$(deriveJSON defaultOptions ''SlotType)

instance ToJSONKey SlotType
instance FromJSONKey SlotType
