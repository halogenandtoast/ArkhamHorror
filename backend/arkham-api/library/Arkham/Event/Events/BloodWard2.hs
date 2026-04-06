module Arkham.Event.Events.BloodWard2 (bloodWard2) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Window (getAttackDetails)

newtype BloodWard2 = BloodWard2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodWard2 :: EventCard BloodWard2
bloodWard2 = event BloodWard2 Cards.bloodWard2

instance RunMessage BloodWard2 where
  runMessage msg e@(BloodWard2 attrs) = runQueueT $ case msg of
    PlayThisEvent _ (is attrs -> True) -> do
      let attack = getAttackDetails attrs.windows
      cancelAttack attrs attack
      for_ attack.investigator \iid -> healDamage iid attrs 2
      pure e
    _ -> BloodWard2 <$> liftRunMessage msg attrs
