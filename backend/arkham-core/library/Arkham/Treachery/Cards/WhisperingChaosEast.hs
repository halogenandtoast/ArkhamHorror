module Arkham.Treachery.Cards.WhisperingChaosEast (whisperingChaosEast, WhisperingChaosEast (..)) where

import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhisperingChaosEast = WhisperingChaosEast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whisperingChaosEast :: TreacheryCard WhisperingChaosEast
whisperingChaosEast = treachery WhisperingChaosEast Cards.whisperingChaosEast

instance RunMessage WhisperingChaosEast where
  runMessage msg t@(WhisperingChaosEast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    _ -> WhisperingChaosEast <$> liftRunMessage msg attrs
