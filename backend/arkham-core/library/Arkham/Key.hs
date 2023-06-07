module Arkham.Key where

import Arkham.Prelude

data ArkhamKey
  = SkullKey
  | CultistKey
  | TabletKey
  | ElderThingKey
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
