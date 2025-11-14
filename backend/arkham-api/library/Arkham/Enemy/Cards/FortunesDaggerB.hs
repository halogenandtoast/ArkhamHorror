module Arkham.Enemy.Cards.FortunesDaggerB (fortunesDaggerB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype FortunesDaggerB = FortunesDaggerB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortunesDaggerB :: EnemyCard FortunesDaggerB
fortunesDaggerB = enemy FortunesDaggerB Cards.fortunesDaggerB (4, Static 3, 2) (1, 1)

instance HasAbilities FortunesDaggerB where
  getAbilities (FortunesDaggerB a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage FortunesDaggerB where
  runMessage msg e@(FortunesDaggerB attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      checkGameIcons attrs iid NoMulligan 2
      pure e
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      focusCards cards $ continue_ iid
      n <- cards & mapMaybeM toPlayingCard <&> count ((== Clubs) . (.suit))
      when (n > 0) $ enemyAttackModifier (attrs.ability 1) attrs (DamageDealt n)
      pure e
    _ -> FortunesDaggerB <$> liftRunMessage msg attrs
