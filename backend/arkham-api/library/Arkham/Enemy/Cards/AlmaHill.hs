module Arkham.Enemy.Cards.AlmaHill (almaHill) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype AlmaHill = AlmaHill EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

almaHill :: EnemyCard AlmaHill
almaHill = enemyWith AlmaHill Cards.almaHill (3, Static 3, 3) (0, 2) $ spawnAtL ?~ "Southside"

instance HasAbilities AlmaHill where
  getAbilities (AlmaHill attrs) = extend attrs [restricted attrs 1 OnSameLocation parleyAction_]

instance RunMessage AlmaHill where
  runMessage msg e@(AlmaHill attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCardsEdit iid attrs 3 (`andThen` AddToVictory (toTarget attrs))
      pure e
    _ -> AlmaHill <$> liftRunMessage msg attrs
