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
  | TidalTunnelDeck -- The Pit of Dispair
  | LeadsDeck -- The Vanishing of Elina Harper, Murder at the Excelsior Hotel
  | RoadDeck -- Horror in High Gear
  | TekeliliDeck -- Edge of the Earth
  deriving stock (Show, Ord, Eq, Data)

instance ToDisplay ScenarioDeckKey where
  toDisplay = \case
    CultistDeck -> "Cultist"
    ExhibitDeck -> "Exhibit"
    PotentialSacrifices -> "Potential Sacrifices"
    LunaticsDeck -> "Lunatics"
    MonstersDeck -> "Monsters"
    CatacombsDeck -> "Catacombs"
    ExplorationDeck -> "Exploration"
    UnknownPlacesDeck -> "Unknown Places"
    CosmosDeck -> "Cosmos"
    TidalTunnelDeck -> "Tidal Tunnels"
    LeadsDeck -> "Leads"
    RoadDeck -> "Road"
    TekeliliDeck -> "Tekeli-li"

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
