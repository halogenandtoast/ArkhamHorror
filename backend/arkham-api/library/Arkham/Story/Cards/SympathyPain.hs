module Arkham.Story.Cards.SympathyPain (sympathyPain) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Helpers
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype SympathyPain = SympathyPain StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sympathyPain :: StoryCard SympathyPain
sympathyPain = story SympathyPain Cards.sympathyPain

instance RunMessage SympathyPain where
  runMessage msg s@(SympathyPain attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      sharedADeepPain <- remembered SharedADeepPain
      slainForeman <- selectJust $ enemyIs Enemies.slainForemanSympathyPain
      if sharedADeepPain
        then do
          removeEnemy slainForeman
          addToVictory iid attrs
        else do
          removeStory attrs
          defeatWindows <- lift $ cancelEnemyDefeatCapture slainForeman
          checkWindows defeatWindows
          healAllDamage attrs slainForeman
          disengageFromAll slainForeman
          exhaustThis slainForeman
      pure s
    _ -> SympathyPain <$> liftRunMessage msg attrs
