module Arkham.Treachery.Cards.WhisperingChaosWest (whisperingChaosWest, WhisperingChaosWest (..)) where

import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WhisperingChaosWest = WhisperingChaosWest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whisperingChaosWest :: TreacheryCard WhisperingChaosWest
whisperingChaosWest = treachery WhisperingChaosWest Cards.whisperingChaosWest

instance RunMessage WhisperingChaosWest where
  runMessage msg t@(WhisperingChaosWest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    _ -> WhisperingChaosWest <$> liftRunMessage msg attrs
