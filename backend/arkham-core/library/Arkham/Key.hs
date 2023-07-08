module Arkham.Key where

import Arkham.Prelude

data ArkhamKey
  = SkullKey
  | CultistKey
  | TabletKey
  | ElderThingKey
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

keyName :: ArkhamKey -> Text
keyName = \case
  SkullKey -> "Skull"
  CultistKey -> "Cultist"
  TabletKey -> "Tablet"
  ElderThingKey -> "ElderThing"
