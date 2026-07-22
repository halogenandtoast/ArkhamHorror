module Arkham.Enemy.Cards.GangEnforcer (gangEnforcer) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype GangEnforcer = GangEnforcer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gangEnforcer :: EnemyCard GangEnforcer
gangEnforcer = enemy GangEnforcer Cards.gangEnforcer & setPrey MostClues

instance HasAbilities GangEnforcer where
  getAbilities (GangEnforcer attrs) =
    extend1 attrs
      $ groupLimit PerRound
      $ mkAbility attrs 1
      $ forced
      $ EnemyAttacked #after (at_ $ locationWithEnemy attrs) AnySource (#criminal <> not_ (be attrs))

instance RunMessage GangEnforcer where
  runMessage msg e@(GangEnforcer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readyThis attrs
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> GangEnforcer <$> liftRunMessage msg attrs
