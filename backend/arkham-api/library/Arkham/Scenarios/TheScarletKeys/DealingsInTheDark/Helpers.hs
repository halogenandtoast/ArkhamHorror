module Arkham.Scenarios.TheScarletKeys.DealingsInTheDark.Helpers where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Story.CardDefs.TheScarletKeys.DealingsInTheDark qualified as Stories
import Arkham.Story.Types (Field (..))

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "dealingsInTheDark" a

getCluesPossesedByTheCult :: HasGame m => m Int
getCluesPossesedByTheCult = do
  cultistClues <- selectSum EnemyClues (enemy_ #cultist)
  cluesUnveiled <- maybe (pure 0) (field StoryClues) =<< selectOne (storyIs Stories.theUnveiling)
  pure $ cultistClues + cluesUnveiled
