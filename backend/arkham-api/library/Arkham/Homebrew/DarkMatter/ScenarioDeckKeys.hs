{-# LANGUAGE TemplateHaskell #-}

{- | Scenario-deck keys owned by the Dark Matter campaign.

'declareHomebrewScenarioDeckKeys' generates a bidirectional pattern synonym for
each name over the core 'Arkham.Scenario.Deck.HomebrewScenarioDeckKey' escape
hatch. Compilation fails if a name already exists as a core scenario-deck key.
-}
module Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (module Arkham.Homebrew.DarkMatter.ScenarioDeckKeys) where

import Arkham.Homebrew.TH (declareHomebrewScenarioDeckKeys)

declareHomebrewScenarioDeckKeys
  [ "ScanningDeck"
  ]
