module Arkham.Scenarios.TheWagesOfSin.Helpers
where

import Arkham.Prelude

import Arkham.Card.EncounterCard
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Control.Lens (non, _2)

getSpectralDiscards :: (HasGame m) => m [EncounterCard]
getSpectralDiscards =
  scenarioFieldMap ScenarioEncounterDecks (view (at SpectralEncounterDeck . non (Deck [], []) . _2))
