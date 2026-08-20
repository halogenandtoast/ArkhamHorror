module Arkham.Story.Cards.TheScarletKeys.ShadesOfSuffering.PlayfulShadows (playfulShadows) where

import Arkham.Enemy.CardDefs.TheScarletKeys.ShadesOfSuffering qualified as Enemies
import Arkham.Enemy.Helpers
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TheScarletKeys.ShadesOfSuffering qualified as Cards
import Arkham.Story.Import.Lifted

newtype PlayfulShadows = PlayfulShadows StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

playfulShadows :: StoryCard PlayfulShadows
playfulShadows = story PlayfulShadows Cards.playfulShadows

instance RunMessage PlayfulShadows where
  runMessage msg s@(PlayfulShadows attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      embarassedTheConsulate <- remembered EmbarrassedTheConsulate
      uncannyShadow <- selectJust $ enemyIs Enemies.uncannyShadowPlayfulShadows
      if embarassedTheConsulate
        then do
          removeEnemy uncannyShadow
          addToVictory iid attrs
        else do
          removeStory attrs
          defeatWindows <- lift $ cancelEnemyDefeatCapture uncannyShadow
          checkWindows defeatWindows
          healAllDamage attrs uncannyShadow
          disengageFromAll uncannyShadow
          exhaustWith attrs uncannyShadow
      pure s
    _ -> PlayfulShadows <$> liftRunMessage msg attrs
