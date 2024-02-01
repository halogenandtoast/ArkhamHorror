{-# LANGUAGE TemplateHaskell #-}

module Arkham.SlotType where

import Arkham.Prelude

import Data.Aeson.TH
import GHC.OverloadedLabels

data SlotType
  = HandSlot
  | BodySlot
  | AllySlot
  | AccessorySlot
  | ArcaneSlot
  | TarotSlot
  deriving stock (Show, Ord, Eq, Bounded, Enum, Data, Generic)
  deriving anyclass (NoThunks)

instance IsLabel "hand" SlotType where
  fromLabel = HandSlot

instance IsLabel "body" SlotType where
  fromLabel = BodySlot

instance IsLabel "ally" SlotType where
  fromLabel = AllySlot

instance IsLabel "accessory" SlotType where
  fromLabel = AccessorySlot

instance IsLabel "arcane" SlotType where
  fromLabel = ArcaneSlot

instance IsLabel "tarot" SlotType where
  fromLabel = TarotSlot

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
