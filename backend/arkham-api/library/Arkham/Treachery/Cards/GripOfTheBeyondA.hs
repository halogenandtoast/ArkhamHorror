module Arkham.Treachery.Cards.GripOfTheBeyondA (gripOfTheBeyondA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GripOfTheBeyondA = GripOfTheBeyondA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gripOfTheBeyondA :: TreacheryCard GripOfTheBeyondA
gripOfTheBeyondA = treachery GripOfTheBeyondA Cards.gripOfTheBeyondA

instance RunMessage GripOfTheBeyondA where
  runMessage msg t@(GripOfTheBeyondA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GripOfTheBeyondA <$> liftRunMessage msg attrs
