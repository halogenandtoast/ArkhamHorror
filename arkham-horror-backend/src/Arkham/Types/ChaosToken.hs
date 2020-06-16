module Arkham.Types.ChaosToken where

import ClassyPrelude
import Data.Aeson

data ArkhamChaosToken
  = PlusOne
  | Zero
  | MinusOne
  | MinusTwo
  | MinusThree
  | MinusFour
  | MinusFive
  | MinusSix
  | MinusSeven
  | MinusEight
  | Skull
  | Cultist
  | Tablet
  | ElderThing
  | AutoFail
  | ElderSign
  deriving stock (Generic)

instance ToJSON ArkhamChaosToken where
  toJSON PlusOne = "+1"
  toJSON Zero = "0"
  toJSON MinusOne = "-1"
  toJSON MinusTwo = "-2"
  toJSON MinusThree = "-3"
  toJSON MinusFour = "-4"
  toJSON MinusFive = "-5"
  toJSON MinusSix = "-6"
  toJSON MinusSeven = "-7"
  toJSON MinusEight = "-8"
  toJSON Skull = "skull"
  toJSON Cultist = "cultist"
  toJSON Tablet = "tablet"
  toJSON ElderThing = "elderthing"
  toJSON AutoFail = "autofail"
  toJSON ElderSign = "eldersign"
