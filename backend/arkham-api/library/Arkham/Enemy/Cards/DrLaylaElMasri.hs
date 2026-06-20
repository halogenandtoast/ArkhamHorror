module Arkham.Enemy.Cards.DrLaylaElMasri (drLaylaElMasri) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype DrLaylaElMasri = DrLaylaElMasri EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drLaylaElMasri :: EnemyCard DrLaylaElMasri
drLaylaElMasri =
  enemyWith DrLaylaElMasri Cards.drLaylaElMasri
    $ spawnAtL
    ?~ SpawnAt "Expedition Camp"

instance HasAbilities DrLaylaElMasri where
  getAbilities (DrLaylaElMasri a) =
    extend1 a
      $ restricted a 1 OnSameLocation
      $ parleyAction (GroupSkillIconCost 4 (singleton #willpower) (locationWithEnemy a))

instance RunMessage DrLaylaElMasri where
  runMessage msg e@(DrLaylaElMasri attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure e
    _ -> DrLaylaElMasri <$> liftRunMessage msg attrs
