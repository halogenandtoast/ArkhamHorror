module Arkham.Story.Cards.TheScarletKeys.ShadesOfSuffering.ExhumeTheBones (exhumeTheBones) where

import Arkham.Enemy.CardDefs.TheScarletKeys.ShadesOfSuffering qualified as Enemies
import Arkham.Enemy.Helpers
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.CardDefs.TheScarletKeys.ShadesOfSuffering qualified as Cards
import Arkham.Story.Import.Lifted

newtype ExhumeTheBones = ExhumeTheBones StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhumeTheBones :: StoryCard ExhumeTheBones
exhumeTheBones = story ExhumeTheBones Cards.exhumeTheBones

instance RunMessage ExhumeTheBones where
  runMessage msg s@(ExhumeTheBones attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      foundHiddenBones <- remembered FoundHiddenBones
      buriedMiner <- selectJust $ enemyIs Enemies.buriedMinerExhumeTheBones
      if foundHiddenBones
        then do
          removeEnemy buriedMiner
          addToVictory iid attrs
        else do
          removeStory attrs
          defeatWindows <- lift $ cancelEnemyDefeatCapture buriedMiner
          checkWindows defeatWindows
          healAllDamage attrs buriedMiner
          disengageFromAll buriedMiner
          exhaustWith attrs buriedMiner
      pure s
    _ -> ExhumeTheBones <$> liftRunMessage msg attrs
