module Arkham.Types.Token where

import ClassyPrelude
import Data.Aeson

data TokenValue = TokenValue Token Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

tokenValue :: TokenValue -> Int
tokenValue (TokenValue _ n) = n

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
  | CompositeToken [Token]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
