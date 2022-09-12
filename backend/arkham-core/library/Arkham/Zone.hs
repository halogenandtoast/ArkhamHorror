module Arkham.Zone where

import Arkham.Prelude

import Data.Aeson.Types

data Zone
  = FromHand
  | FromDeck
  | FromTopOfDeck Int
  | FromDiscard
  | FromPlay
  | FromOutOfPlay
  | FromCollection
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data ScenarioZone
  = FromEncounterDeck
  | FromEncounterDiscard
  | FromVoid
  | FromOutOfPlayAreas
  | FromVictoryDisplay
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance ToJSONKey Zone where
  toJSONKey = toJSONKeyText textKey
   where
    textKey = \case
      FromTopOfDeck _ -> "FromDeck"
      other -> tshow other

instance FromJSONKey Zone where
  fromJSONKey = FromJSONKeyText \case
    "FromHand" -> FromHand
    "FromDeck" -> FromDeck
    "FromDiscard" -> FromDiscard
    "FromPlay" -> FromPlay
    "FromOutOfPlay" -> FromOutOfPlay
    "FromCollection" -> FromCollection
    other -> error ("Unhandled FromJSONKey for zone" <> show other)
