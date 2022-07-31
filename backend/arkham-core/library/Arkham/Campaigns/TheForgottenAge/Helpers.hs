module Arkham.Campaigns.TheForgottenAge.Helpers where

import Arkham.Prelude

import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Scenario.Types

getVengeanceInVictoryDisplay :: (HasGame m, Monad m) => m Int
getVengeanceInVictoryDisplay =
  sum
    . map (fromMaybe 0 . cdVictoryPoints . toCardDef)
    <$> scenarioField ScenarioVictoryDisplay

getExplorationDeck :: (HasGame m, Monad m) => m [Card]
getExplorationDeck = scenarioFieldMap
  ScenarioDecks
  (findWithDefault (error "missing deck") ExplorationDeck)
