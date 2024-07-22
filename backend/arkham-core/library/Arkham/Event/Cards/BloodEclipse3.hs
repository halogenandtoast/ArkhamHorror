module Arkham.Event.Cards.BloodEclipse3 (bloodEclipse3, BloodEclipse3 (..)) where

import Arkham.Aspect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Modifier

newtype BloodEclipse3 = BloodEclipse3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodEclipse3 :: EventCard BloodEclipse3
bloodEclipse3 = event BloodEclipse3 Cards.bloodEclipse3

instance RunMessage BloodEclipse3 where
  runMessage msg e@(BloodEclipse3 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let n = attrs.payment.investigatorDamage
      sid <- getRandom
      skillTestModifiers sid attrs iid [DamageDealt n, SkillModifier #willpower n]
      pushAllM
        $ leftOr
        <$> aspect iid attrs (#willpower `InsteadOf` #combat) (mkChooseFight sid iid attrs)
      pure e
    _ -> BloodEclipse3 <$> liftRunMessage msg attrs
