module Arkham.Enemy.Cards.VictoriaDevereux (victoriaDevereux) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype VictoriaDevereux = VictoriaDevereux EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victoriaDevereux :: EnemyCard VictoriaDevereux
victoriaDevereux =
  enemyWith VictoriaDevereux Cards.victoriaDevereux (3, Static 3, 2) (1, 0) (spawnAtL ?~ "Northside")

instance HasAbilities VictoriaDevereux where
  getAbilities (VictoriaDevereux a) =
    extend1 a $ restricted a 1 OnSameLocation $ parleyAction (ResourceCost 5)

instance RunMessage VictoriaDevereux where
  runMessage msg e@(VictoriaDevereux attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure e
    _ -> VictoriaDevereux <$> liftRunMessage msg attrs
