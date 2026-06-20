module Arkham.Enemy.Cards.StarVampire (starVampire) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype StarVampire = StarVampire EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starVampire :: EnemyCard StarVampire
starVampire =
  enemy StarVampire Cards.starVampire
    & setPrey MostRemainingHealth

instance HasAbilities StarVampire where
  getAbilities (StarVampire a) =
    extend1 a
      $ restricted a 1 (thisExists a EnemyWithAnyDamage)
      $ forced
      $ EnemyAttacks #after Anyone AnyEnemyAttack (be a)

instance RunMessage StarVampire where
  runMessage msg e@(StarVampire attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      healDamage attrs (attrs.ability 1) 1
      pure e
    _ -> StarVampire <$> liftRunMessage msg attrs
