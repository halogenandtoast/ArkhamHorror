module Arkham.Story.Cards.ALostMemento (aLostMemento) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Helpers
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype ALostMemento = ALostMemento StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aLostMemento :: StoryCard ALostMemento
aLostMemento = story ALostMemento Cards.aLostMemento

instance RunMessage ALostMemento where
  runMessage msg s@(ALostMemento attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      foundACheapMemento <- remembered FoundACheapMemento
      buriedMiner <- selectJust $ enemyIs Enemies.buriedMinerALostMemento
      if foundACheapMemento
        then do
          removeEnemy buriedMiner
          addToVictory iid attrs
        else do
          removeStory attrs
          defeatWindows <- lift $ cancelEnemyDefeatCapture buriedMiner
          checkWindows defeatWindows
          healAllDamage attrs buriedMiner
          disengageFromAll buriedMiner
          exhaustThis buriedMiner
      pure s
    _ -> ALostMemento <$> liftRunMessage msg attrs
