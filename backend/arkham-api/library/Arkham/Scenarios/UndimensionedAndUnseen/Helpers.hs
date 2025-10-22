module Arkham.Scenarios.UndimensionedAndUnseen.Helpers where

import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Card (Card)
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.Prelude
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "undimensionedAndUnseen" a

broodTitle :: Text
broodTitle = nameTitle . toName $ Cards.broodOfYogSothoth

getMatchingBroodOfYogSothoth :: (HasGame m, Tracing m) => EnemyMatcher -> m [EnemyId]
getMatchingBroodOfYogSothoth matcher = select $ InPlayEnemy $ EnemyWithTitle broodTitle <> matcher

getBroodOfYogSothoth :: (HasGame m, Tracing m) => m [EnemyId]
getBroodOfYogSothoth = select $ InPlayEnemy $ EnemyWithTitle broodTitle

getSetAsideBroodOfYogSothoth :: (HasGame m, Tracing m) => m [Card]
getSetAsideBroodOfYogSothoth = getSetAsideCardsMatching $ CardWithTitle broodTitle
