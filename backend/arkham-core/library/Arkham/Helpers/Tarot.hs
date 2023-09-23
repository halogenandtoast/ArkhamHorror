module Arkham.Helpers.Tarot where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Scenario.Types
import Arkham.Tarot

affectedByTarot :: HasGame m => InvestigatorId -> TarotCard -> m Bool
affectedByTarot iid tarotCard = do
  tarotCards <- scenarioField ScenarioTarotCards
  pure
    $ tarotCard
    `elem` findWithDefault [] GlobalTarot tarotCards
    || tarotCard
    `elem` findWithDefault [] (InvestigatorTarot iid) tarotCards
