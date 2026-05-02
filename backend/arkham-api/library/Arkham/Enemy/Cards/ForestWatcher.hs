module Arkham.Enemy.Cards.ForestWatcher (forestWatcher) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype ForestWatcher = ForestWatcher EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forestWatcher :: EnemyCard ForestWatcher
forestWatcher =
  enemyWith ForestWatcher Cards.forestWatcher (1, Static 2, 3) (1, 0)
    $ spawnAtL
    ?~ SpawnAtFirst [SpawnAt EmptyLocation, SpawnAt Anywhere]

instance HasAbilities ForestWatcher where
  getAbilities (ForestWatcher a) =
    extend1 a $ restricted a 1 CanPlaceDoomOnThis $ forced $ EnemyEntersPlay #after (be a)

instance RunMessage ForestWatcher where
  runMessage msg e@(ForestWatcher attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom attrs attrs 1
      pure e
    _ -> ForestWatcher <$> liftRunMessage msg attrs
