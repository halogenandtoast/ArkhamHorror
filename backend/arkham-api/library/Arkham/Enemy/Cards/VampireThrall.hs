module Arkham.Enemy.Cards.VampireThrall (vampireThrall) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype VampireThrall = VampireThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vampireThrall :: EnemyCard VampireThrall
vampireThrall =
  enemy VampireThrall Cards.vampireThrall (2, Static 3, 2) (1, 0)
    & setPrey LowestRemainingHealth

instance HasAbilities VampireThrall where
  getAbilities (VampireThrall a) =
    extend1 a
      $ restricted a 1 (thisExists a EnemyWithAnyDamage)
      $ forced
      $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage VampireThrall where
  runMessage msg e@(VampireThrall attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> VampireThrall <$> liftRunMessage msg attrs
