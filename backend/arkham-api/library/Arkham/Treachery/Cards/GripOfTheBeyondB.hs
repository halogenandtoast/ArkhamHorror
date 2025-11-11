module Arkham.Treachery.Cards.GripOfTheBeyondB (gripOfTheBeyondB) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GripOfTheBeyondB = GripOfTheBeyondB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gripOfTheBeyondB :: TreacheryCard GripOfTheBeyondB
gripOfTheBeyondB = treachery GripOfTheBeyondB Cards.gripOfTheBeyondB

instance RunMessage GripOfTheBeyondB where
  runMessage msg t@(GripOfTheBeyondB attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GripOfTheBeyondB <$> liftRunMessage msg attrs
