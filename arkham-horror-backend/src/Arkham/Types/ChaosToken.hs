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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

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

instance FromJSON ArkhamChaosToken where
  parseJSON (String x) = case x of
    "+1" -> pure PlusOne
    "0" -> pure Zero
    "-1" -> pure MinusOne
    "-2" -> pure MinusTwo
    "-3" -> pure MinusThree
    "-4" -> pure MinusFour
    "-5" -> pure MinusFive
    "-6" -> pure MinusSix
    "-7" -> pure MinusSeven
    "-8" -> pure MinusEight
    "skull" -> pure Skull
    "cultist" -> pure Cultist
    "tablet" -> pure Tablet
    "elderthing" -> pure ElderThing
    "autofail" -> pure AutoFail
    "eldersign" -> pure ElderSign
    _ -> mzero
  parseJSON _ = mzero
