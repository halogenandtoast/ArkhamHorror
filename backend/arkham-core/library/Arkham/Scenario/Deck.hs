{-# LANGUAGE TemplateHaskell #-}

module Arkham.Scenario.Deck where

import Arkham.Prelude

import Data.Aeson.TH

data ScenarioDeckKey
  = CultistDeck -- The Midnight Masks
  | ExhibitDeck -- The Miskatonic Museum
  | PotentialSacrifices -- Blood on the Altar
  | LunaticsDeck -- The Unspeakable Oath
  | MonstersDeck -- The Unspeakable Oath
  | CatacombsDeck -- The Pallid Mask
  | ExplorationDeck -- The Untamed Wilds
  | UnknownPlacesDeck -- The Secret Name
  | CosmosDeck -- Before the Black Throne
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''ScenarioDeckKey)

instance ToJSONKey ScenarioDeckKey
instance FromJSONKey ScenarioDeckKey

data ScenarioEncounterDeckKey
  = RegularEncounterDeck
  | SpectralEncounterDeck -- The Wages of Sin
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''ScenarioEncounterDeckKey)

instance ToJSONKey ScenarioEncounterDeckKey
instance FromJSONKey ScenarioEncounterDeckKey
