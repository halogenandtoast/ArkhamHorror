module Arkham.Enemy.Cards.SpawnOfHali (spawnOfHali) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype SpawnOfHali = SpawnOfHali EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spawnOfHali :: EnemyCard SpawnOfHali
spawnOfHali =
  enemy SpawnOfHali Cards.spawnOfHali (4, Static 4, 2) (1, 2)
    & setPrey MostHorror

instance HasAbilities SpawnOfHali where
  getAbilities (SpawnOfHali a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult
        #after
        You
        (WhileEvadingAnEnemy $ be a)
        (SuccessResult $ LessThanOrEqualTo $ Static 2)

instance RunMessage SpawnOfHali where
  runMessage msg e@(SpawnOfHali attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure e
    _ -> SpawnOfHali <$> liftRunMessage msg attrs
