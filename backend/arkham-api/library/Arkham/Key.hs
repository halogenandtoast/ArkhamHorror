module Arkham.Key where

import Arkham.Prelude

data ArkhamKey
  = SkullKey
  | CultistKey
  | TabletKey
  | ElderThingKey
  | RedKey
  | BlueKey
  | GreenKey
  | YellowKey
  | PurpleKey
  | BlackKey
  | WhiteKey
  | UnrevealedKey ArkhamKey
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

keyName :: ArkhamKey -> Text
keyName = \case
  SkullKey -> "Skull"
  CultistKey -> "Cultist"
  TabletKey -> "Tablet"
  ElderThingKey -> "ElderThing"
  RedKey -> "Red"
  BlueKey -> "Blue"
  GreenKey -> "Green"
  YellowKey -> "Yellow"
  PurpleKey -> "Purple"
  BlackKey -> "Black"
  WhiteKey -> "White"
  UnrevealedKey _ -> "Unrevealed"
