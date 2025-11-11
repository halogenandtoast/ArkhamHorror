module Arkham.Treachery.Cards.GripOfTheBeyondC (gripOfTheBeyondC) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype GripOfTheBeyondC = GripOfTheBeyondC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gripOfTheBeyondC :: TreacheryCard GripOfTheBeyondC
gripOfTheBeyondC = treachery GripOfTheBeyondC Cards.gripOfTheBeyondC

instance RunMessage GripOfTheBeyondC where
  runMessage msg t@(GripOfTheBeyondC attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> GripOfTheBeyondC <$> liftRunMessage msg attrs
