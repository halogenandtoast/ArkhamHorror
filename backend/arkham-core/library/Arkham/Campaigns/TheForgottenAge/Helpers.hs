module Arkham.Campaigns.TheForgottenAge.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Matcher
import Arkham.Scenario.Types
import Arkham.Treachery.Cards qualified as Treacheries

getVengeanceInVictoryDisplay :: (HasGame m, Monad m) => m Int
getVengeanceInVictoryDisplay =
  sum
    . map (fromMaybe 0 . cdVictoryPoints . toCardDef)
    <$> scenarioField ScenarioVictoryDisplay

getExplorationDeck :: (HasGame m, Monad m) => m [Card]
getExplorationDeck = scenarioFieldMap
  ScenarioDecks
  (findWithDefault (error "missing deck") ExplorationDeck)

getIsPoisoned :: (HasGame m, Monad m) => InvestigatorId -> m Bool
getIsPoisoned iid =
  selectAny $ treacheryIs Treacheries.poisoned <> treacheryInThreatAreaOf iid

getSetAsidePoisoned :: (HasGame m, Monad m) => m Card
getSetAsidePoisoned =
  fromJustNote "not enough poison cards"
    . find ((== Treacheries.poisoned) . toCardDef)
    <$> scenarioField ScenarioSetAsideCards

