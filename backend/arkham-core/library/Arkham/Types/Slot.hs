module Arkham.Types.Slot where

import Arkham.Json
import Arkham.Types.AssetId
import Arkham.Types.Trait
import ClassyPrelude

newtype SlotRestriction = TraitSlotRestriction Trait
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SlotContents = AssetSlotContents AssetId [Trait]
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Slot = Slot (Maybe SlotContents) | TraitRestrictedSlot Trait (Maybe SlotContents)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SlotType
  = HandSlot
  | BodySlot
  | AllySlot
  | AccessorySlot
  | ArcaneSlot
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
