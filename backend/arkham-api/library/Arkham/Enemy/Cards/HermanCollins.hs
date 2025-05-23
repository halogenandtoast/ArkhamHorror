module Arkham.Enemy.Cards.HermanCollins (hermanCollins) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype HermanCollins = HermanCollins EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hermanCollins :: EnemyCard HermanCollins
hermanCollins = enemyWith HermanCollins Cards.hermanCollins (3, Static 4, 4) (1, 1) (spawnAtL ?~ "Graveyard")

instance HasAbilities HermanCollins where
  getAbilities (HermanCollins a) =
    extend1 a $ restricted a 1 OnSameLocation $ parleyAction (HandDiscardCost 4 #any)

instance RunMessage HermanCollins where
  runMessage msg e@(HermanCollins attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure e
    _ -> HermanCollins <$> liftRunMessage msg attrs
