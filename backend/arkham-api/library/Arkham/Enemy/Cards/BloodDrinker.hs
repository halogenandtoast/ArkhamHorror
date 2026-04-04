module Arkham.Enemy.Cards.BloodDrinker (bloodDrinker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype BloodDrinker = BloodDrinker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodDrinker :: EnemyCard BloodDrinker
bloodDrinker = enemy BloodDrinker Cards.bloodDrinker (2, Static 3, 4) (1, 1)

instance HasAbilities BloodDrinker where
  getAbilities (BloodDrinker a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage BloodDrinker where
  runMessage msg e@(BloodDrinker attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> BloodDrinker <$> liftRunMessage msg attrs
