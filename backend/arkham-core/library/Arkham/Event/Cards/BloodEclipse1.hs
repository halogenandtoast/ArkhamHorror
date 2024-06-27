module Arkham.Event.Cards.BloodEclipse1 (bloodEclipse1, BloodEclipse1 (..)) where

import Arkham.Aspect
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Modifier

newtype BloodEclipse1 = BloodEclipse1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodEclipse1 :: EventCard BloodEclipse1
bloodEclipse1 = event BloodEclipse1 Cards.bloodEclipse1

instance RunMessage BloodEclipse1 where
  runMessage msg e@(BloodEclipse1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      skillTestModifiers attrs iid [DamageDealt 2, SkillModifier #willpower 2]
      pushAllM $ leftOr <$> aspect iid attrs (#willpower `InsteadOf` #combat) (mkChooseFight iid attrs)
      pure e
    _ -> BloodEclipse1 <$> lift (runMessage msg attrs)
