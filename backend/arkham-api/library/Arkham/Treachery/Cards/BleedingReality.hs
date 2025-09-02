module Arkham.Treachery.Cards.BleedingReality (bleedingReality) where

import Arkham.Helpers.Act
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BleedingReality = BleedingReality TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bleedingReality :: TreacheryCard BleedingReality
bleedingReality = treachery BleedingReality Cards.bleedingReality

instance RunMessage BleedingReality where
  runMessage msg t@(BleedingReality attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- getCurrentActStep
      sid <- getRandom
      revelationSkillTest sid iid attrs #intellect (Fixed n)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) (min 3 -> n) -> do
      randomDiscardN iid attrs n
      pure t
    _ -> BleedingReality <$> liftRunMessage msg attrs
