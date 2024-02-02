module Arkham.Key where

import Arkham.Prelude

data ArkhamKey
  = SkullKey
  | CultistKey
  | TabletKey
  | ElderThingKey
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (NoThunks, NFData)
  deriving anyclass (ToJSON, FromJSON)

keyName :: ArkhamKey -> Text
keyName = \case
  SkullKey -> "Skull"
  CultistKey -> "Cultist"
  TabletKey -> "Tablet"
  ElderThingKey -> "ElderThing"
