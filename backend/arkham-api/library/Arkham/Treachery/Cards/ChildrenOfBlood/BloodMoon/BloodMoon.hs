module Arkham.Treachery.Cards.ChildrenOfBlood.BloodMoon.BloodMoon (bloodMoon) where

import Arkham.Treachery.CardDefs.ChildrenOfBlood.BloodMoon qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype BloodMoon = BloodMoon TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bloodMoon :: TreacheryCard BloodMoon
bloodMoon = treachery BloodMoon Cards.bloodMoon

instance RunMessage BloodMoon where
  runMessage msg t@(BloodMoon attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> BloodMoon <$> liftRunMessage msg attrs
