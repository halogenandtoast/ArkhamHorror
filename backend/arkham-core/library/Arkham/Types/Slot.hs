module Arkham.Types.Slot where

import Arkham.Json
import ClassyPrelude

data Slot
  = HandSlot
  | TomeSlot
  | AllySlot
  | AccessorySlot
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
