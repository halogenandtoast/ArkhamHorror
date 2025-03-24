module Arkham.Enemy.Cards.PeterWarren (peterWarren) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted

newtype PeterWarren = PeterWarren EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterWarren :: EnemyCard PeterWarren
peterWarren =
  enemyWith
    PeterWarren
    Cards.peterWarren
    (2, Static 3, 3)
    (1, 0)
    (spawnAtL ?~ "Miskatonic University")

instance HasAbilities PeterWarren where
  getAbilities (PeterWarren attrs) =
    extend1 attrs
      $ restricted attrs 1 OnSameLocation
      $ parleyAction (ClueCost (Static 2))

instance RunMessage PeterWarren where
  runMessage msg e@(PeterWarren attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure e
    _ -> PeterWarren <$> liftRunMessage msg attrs
