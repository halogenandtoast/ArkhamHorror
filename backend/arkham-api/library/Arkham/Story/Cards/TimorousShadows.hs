module Arkham.Story.Cards.TimorousShadows (timorousShadows) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Helpers
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype TimorousShadows = TimorousShadows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timorousShadows :: StoryCard TimorousShadows
timorousShadows = story TimorousShadows Cards.timorousShadows

instance RunMessage TimorousShadows where
  runMessage msg s@(TimorousShadows attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      peeredBeyond <- remembered PeeredBeyond
      uncannyShadow <- selectJust $ enemyIs Enemies.uncannyShadowTimorousShadows
      if peeredBeyond
        then do
          removeEnemy uncannyShadow
          addToVictory iid attrs
        else do
          removeStory attrs
          defeatWindows <- lift $ cancelEnemyDefeatCapture uncannyShadow
          checkWindows defeatWindows
          healAllDamage attrs uncannyShadow
          disengageFromAll uncannyShadow
          exhaustThis uncannyShadow
      pure s
    _ -> TimorousShadows <$> liftRunMessage msg attrs
