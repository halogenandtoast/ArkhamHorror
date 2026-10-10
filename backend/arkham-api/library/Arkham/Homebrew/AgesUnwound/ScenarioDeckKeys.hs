{-# LANGUAGE TemplateHaskell #-}

{- | Scenario-deck keys owned by the Ages Unwound campaign.

'declareHomebrewScenarioDeckKeys' generates a bidirectional pattern synonym for
each name over the core 'Arkham.Scenario.Deck.HomebrewScenarioDeckKey' escape
hatch. Compilation fails if a name already exists as a core scenario-deck key.
-}
module Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (module Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys) where

import Arkham.Homebrew.TH (declareHomebrewScenarioDeckKeys)

declareHomebrewScenarioDeckKeys
  [ -- Scenario I: the face-down stack of Arkham Streets locations you flee through
    "ArkhamStreetsDeck"
  , -- Scenario V: the open-ended Task deck (see the core Task trait)
    "TaskDeck"
  ]
