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
  deriving stock (Show, Ord, Eq)

$(deriveJSON defaultOptions ''ScenarioDeckKey)

instance ToJSONKey ScenarioDeckKey
instance FromJSONKey ScenarioDeckKey
