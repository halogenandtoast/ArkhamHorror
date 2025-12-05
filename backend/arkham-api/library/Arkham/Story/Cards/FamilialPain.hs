module Arkham.Story.Cards.FamilialPain (familialPain) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Helpers
import Arkham.Helpers.Log
import Arkham.Matcher
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype FamilialPain = FamilialPain StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

familialPain :: StoryCard FamilialPain
familialPain = story FamilialPain Cards.familialPain

instance RunMessage FamilialPain where
  runMessage msg s@(FamilialPain attrs) = runQueueT $ case msg of
    ResolveThisStory iid (is attrs -> True) -> do
      tradedForAKitten <- remembered TradedForAKitten
      slainForeman <- selectJust $ enemyIs Enemies.slainForemanFamilialPain
      if tradedForAKitten
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
    _ -> FamilialPain <$> liftRunMessage msg attrs
