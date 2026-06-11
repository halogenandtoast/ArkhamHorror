module Arkham.Scenarios.TheLabyrinthsOfLunacy.Helpers where

import Arkham.Classes.HasGame
import Arkham.Helpers.Scenario (scenarioField, standaloneI18n)
import Arkham.I18n
import Arkham.Prelude
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.TheLabyrinthsOfLunacy.Meta
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "theLabyrinthsOfLunacy" a

getMeta :: (HasGame m, Tracing m) => m Meta
getMeta = toResult <$> scenarioField ScenarioMeta

-- | Which group is currently trapped in the labyrinth.
getGroup :: (HasGame m, Tracing m) => m Group
getGroup = currentGroup <$> getMeta

groupLabel :: HasI18n => Group -> Text
groupLabel g = unscoped $ standaloneI18n "theLabyrinthsOfLunacy" $ scope "group" $ case g of
  GroupA -> "groupA"
  GroupB -> "groupB"
  GroupC -> "groupC"
