module Arkham.Types.Token where

import ClassyPrelude
import Data.Aeson

data Token
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
