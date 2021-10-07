module Arkham.Types.Scenario.Deck where

import Arkham.Prelude

data ScenarioDeckKey
  = CultistDeck
  | ExhibitDeck
  | PotentialSacrifices
  | LunaticsDeck
  | MonstersDeck
  deriving stock (Show, Generic, Eq)
  deriving anyclass (ToJSON, FromJSON, Hashable, ToJSONKey, FromJSONKey)
