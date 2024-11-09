module Arkham.Treachery.Cards.FulfillTheOaths (fulfillTheOaths, FulfillTheOaths (..)) where

import Arkham.Helpers.Act
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FulfillTheOaths = FulfillTheOaths TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fulfillTheOaths :: TreacheryCard FulfillTheOaths
fulfillTheOaths = treachery FulfillTheOaths Cards.fulfillTheOaths

instance RunMessage FulfillTheOaths where
  runMessage msg t@(FulfillTheOaths attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      n <- getCurrentActStep
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      when (n >= 2) do
        sid2 <- getRandom
        revelationSkillTest sid2 iid attrs #combat (Fixed 2)
      when (n >= 3) do
        sid3 <- getRandom
        revelationSkillTest sid3 iid attrs #intellect (Fixed 2)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      pure t
    _ -> FulfillTheOaths <$> liftRunMessage msg attrs
