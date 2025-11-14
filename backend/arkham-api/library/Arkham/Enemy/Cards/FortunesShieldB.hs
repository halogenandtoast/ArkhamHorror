module Arkham.Enemy.Cards.FortunesShieldB (fortunesShieldB) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype FortunesShieldB = FortunesShieldB EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortunesShieldB :: EnemyCard FortunesShieldB
fortunesShieldB = enemy FortunesShieldB Cards.fortunesShieldB (2, Static 4, 3) (1, 1)

instance HasAbilities FortunesShieldB where
  getAbilities (FortunesShieldB a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyTakeDamage #when AnyDamageEffect (be a) AnyValue (SourceOwnedBy You)

instance RunMessage FortunesShieldB where
  runMessage msg e@(FortunesShieldB attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      checkGameIcons attrs iid NoMulligan 1
      pure e
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      focusCards cards $ continue_ iid
      suits <- cards & mapMaybeM toPlayingCard <&> map (.suit)
      when (Hearts `elem` suits) $ reduceDamageTaken (attrs.ability 1) attrs 1
      pure e
    _ -> FortunesShieldB <$> liftRunMessage msg attrs
