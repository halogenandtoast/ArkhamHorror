module Arkham.Enemy.Cards.Acolyte (acolyte) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype Acolyte = Acolyte EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acolyte :: EnemyCard Acolyte
acolyte = enemyWith Acolyte Cards.acolyte (3, Static 1, 2) (1, 0) (spawnAtL ?~ SpawnAt EmptyLocation)

instance HasAbilities Acolyte where
  getAbilities (Acolyte a) =
    extend1 a $ restricted a 1 CanPlaceDoomOnThis $ forced $ EnemySpawns #after Anywhere (be a)

instance RunMessage Acolyte where
  runMessage msg e@(Acolyte attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeDoom attrs attrs 1
      pure e
    _ -> Acolyte <$> liftRunMessage msg attrs
