module Arkham.Enemy.Cards.BillyCooper (billyCooper) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Trait

newtype BillyCooper = BillyCooper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

billyCooper :: EnemyCard BillyCooper
billyCooper = enemyWith BillyCooper Cards.billyCooper (5, Static 4, 2) (2, 0) (spawnAtL ?~ SpawnAt "Easttown")

instance HasAbilities BillyCooper where
  getAbilities (BillyCooper a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ defeated #after
      $ EnemyAt (locationWithEnemy a)
      <> withTrait Monster

instance RunMessage BillyCooper where
  runMessage msg e@(BillyCooper attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      addToVictory attrs
      pure e
    _ -> BillyCooper <$> liftRunMessage msg attrs
