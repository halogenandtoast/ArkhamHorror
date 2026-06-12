module Arkham.Scenarios.RedTideRising.Helpers where

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ standaloneI18n "redTideRising" a

victoryRequiresMysteriousPhoto :: ModifierType
victoryRequiresMysteriousPhoto = ScenarioModifier "victoryRequiresMysteriousPhoto"

wendyAdams :: InvestigatorMatcher
wendyAdams = InvestigatorWithTitle "Wendy Adams"

getWendyAdams :: (HasGame m, Tracing m) => m (Maybe InvestigatorId)
getWendyAdams = selectOne wendyAdams

perPlayerOneHideouts :: [CardDef]
perPlayerOneHideouts = [Locations.innsmouthJail, Locations.shorewardSlums, Locations.esotericOrderOfDagon]

perPlayerTwoHideouts :: [CardDef]
perPlayerTwoHideouts = [Locations.sawboneAlley, Locations.theHouseOnWaterStreet, Locations.newChurchGreen]
