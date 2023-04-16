module Arkham.Scenario.Deck where

import Arkham.Prelude

data ScenarioDeckKey
  = CultistDeck -- The Midnight Masks
  | ExhibitDeck -- The Miskatonic Museum
  | PotentialSacrifices -- Blood on the Altar
  | LunaticsDeck -- The Unspeakable Oath
  | MonstersDeck -- The Unspeakable Oath
  | CatacombsDeck -- The Pallid Mask
  | ExplorationDeck -- The Untamed Wilds
  | UnknownPlacesDeck -- The Secret Name
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)
