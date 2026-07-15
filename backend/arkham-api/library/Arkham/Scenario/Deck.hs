{-# LANGUAGE TemplateHaskell #-}

module Arkham.Scenario.Deck where

import Arkham.Prelude
import Data.Aeson.TH
import Data.Data (dataTypeConstrs, dataTypeOf, fromConstr, showConstr)
import Data.Text qualified as T

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
  | GuestDeck -- The Midwinter Gala
  | RoadDeck -- Horror in High Gear
  | TekeliliDeck -- Edge of the Earth
  | OtherworldDeck -- Without a Trace
  | WoodsDeck -- The Twisted Hollow
  | CavernsDeck -- The Lost Sister
  | EnemyDeck -- The Longest Night
  | AbyssDeck -- Fate of the Vale
  | ReelDeck -- FilmFatale
  | PropsDeck -- Enthralling Encore
  | -- | Open extension point for homebrew scenarios. Do not use directly; each
    -- homebrew campaign owns pattern synonyms over this (see its
    -- @ScenarioDeckKeys.hs@). The 'Text' tag is the key name, so serialization
    -- matches a plain enum constructor and needs no migration.
    HomebrewScenarioDeckKey Text
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
    GuestDeck -> "Guest"
    RoadDeck -> "Road"
    TekeliliDeck -> "Tekeli-li"
    OtherworldDeck -> "Otherworld"
    WoodsDeck -> "Woods"
    CavernsDeck -> "Caverns"
    EnemyDeck -> "Enemy"
    AbyssDeck -> "The Abyss"
    ReelDeck -> "Reel"
    PropsDeck -> "Props"
    HomebrewScenarioDeckKey t -> pack $ splitCamelCase $ unpack $ fromMaybe t (T.stripSuffix "Deck" t)

-- | Core keys serialize as their bare constructor name (as the derived
-- all-nullary encoding did); a 'HomebrewScenarioDeckKey' serializes as its tag,
-- which by construction equals the old constructor name, so saves round-trip.
instance ToJSON ScenarioDeckKey where
  toJSON = \case
    HomebrewScenarioDeckKey t -> toJSON t
    k -> toJSON (tshow k)

instance FromJSON ScenarioDeckKey where
  parseJSON = withText "ScenarioDeckKey" $ \t ->
    pure $ findWithDefault (HomebrewScenarioDeckKey t) t coreScenarioDeckKeyByName

-- | Every non-homebrew key, and a name index for it. If a homebrew key is later
-- promoted to a real constructor, its old @"N"@ data resolves here to the real
-- key (the escape-hatch value never survives a round-trip).
coreScenarioDeckKeys :: [ScenarioDeckKey]
coreScenarioDeckKeys =
  [ fromConstr con
  | con <- dataTypeConstrs (dataTypeOf (HomebrewScenarioDeckKey ""))
  , showConstr con /= "HomebrewScenarioDeckKey"
  ]

coreScenarioDeckKeyByName :: Map Text ScenarioDeckKey
coreScenarioDeckKeyByName = mapFromList [(tshow k, k) | k <- coreScenarioDeckKeys]

instance ToJSONKey ScenarioDeckKey
instance FromJSONKey ScenarioDeckKey

data ScenarioEncounterDeckKey
  = RegularEncounterDeck
  | SpectralEncounterDeck -- The Wages of Sin
  | SetAsideEncounterDeck -- Congress of the Keys
  deriving stock (Show, Ord, Eq, Data)

$(deriveJSON defaultOptions ''ScenarioEncounterDeckKey)

instance ToJSONKey ScenarioEncounterDeckKey
instance FromJSONKey ScenarioEncounterDeckKey
