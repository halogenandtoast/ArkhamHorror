module Arkham.Scenarios.DealingsInTheDark.Helpers where

import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.Query
import Arkham.Enemy.Types (Field (..))
import Arkham.I18n
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Story.Cards qualified as Stories
import Arkham.Story.Types (Field (..))
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "dealingsInTheDark" a

getCluesPossesedByTheCult :: (Tracing m, HasGame m) => m Int
getCluesPossesedByTheCult = do
  cultistClues <- selectSum EnemyClues (InPlayEnemy #cultist)
  cluesUnveiled <- maybe (pure 0) (field StoryClues) =<< selectOne (storyIs Stories.theUnveiling)
  pure $ cultistClues + cluesUnveiled
