module Arkham.Enemy.Cards.FortunesDaggerA (fortunesDaggerA) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype FortunesDaggerA = FortunesDaggerA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortunesDaggerA :: EnemyCard FortunesDaggerA
fortunesDaggerA = enemy FortunesDaggerA Cards.fortunesDaggerA (4, Static 3, 2) (1, 1)

instance HasAbilities FortunesDaggerA where
  getAbilities (FortunesDaggerA a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage FortunesDaggerA where
  runMessage msg e@(FortunesDaggerA attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      checkGameIcons attrs iid NoMulligan 2
      pure e
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      focusCards cards $ continue_ iid
      n <- cards & mapMaybeM toPlayingCard <&> count ((== Clubs) . (.suit))
      when (n > 0) $ enemyAttackModifier (attrs.ability 1) attrs (DamageDealt n)
      pure e
    _ -> FortunesDaggerA <$> liftRunMessage msg attrs
