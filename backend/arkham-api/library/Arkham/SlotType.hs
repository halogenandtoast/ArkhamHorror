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
  | HeadSlot
  deriving stock (Show, Ord, Eq, Bounded, Enum, Data)

instance IsLabel "head" SlotType where
  fromLabel = HeadSlot

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
  HeadSlot -> "Head"

-- | i18n key for a slot type, used with @keyVar "slot"@ + the @\@:{slot}@ linked
-- message syntax in the frontend locales (e.g. base.json's @slot.accessory@).
slotKey :: SlotType -> Text
slotKey = \case
  HandSlot -> "slot.hand"
  BodySlot -> "slot.body"
  AllySlot -> "slot.ally"
  AccessorySlot -> "slot.accessory"
  ArcaneSlot -> "slot.arcane"
  TarotSlot -> "slot.tarot"
  HeadSlot -> "slot.head"

allSlotTypes :: [SlotType]
allSlotTypes = [minBound .. maxBound]

$(deriveJSON defaultOptions ''SlotType)

instance ToJSONKey SlotType
instance FromJSONKey SlotType
