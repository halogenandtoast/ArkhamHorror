module Arkham.Helpers.Tarot where

import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Prelude
import Arkham.Scenario.Types
import Arkham.Tarot
import Arkham.Tracing

affectedByTarot :: (HasGame m, Tracing m) => InvestigatorId -> TarotCard -> m Bool
affectedByTarot iid tarotCard = do
  tarotCards <- scenarioField ScenarioTarotCards
  pure
    $ tarotCard
    `elem` findWithDefault [] GlobalTarot tarotCards
    || tarotCard
    `elem` findWithDefault [] (InvestigatorTarot iid) tarotCards
