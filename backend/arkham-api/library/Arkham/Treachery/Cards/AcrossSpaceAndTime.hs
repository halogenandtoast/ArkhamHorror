module Arkham.Treachery.Cards.AcrossSpaceAndTime ( acrossSpaceAndTime,) where

import Arkham.Treachery.Cards qualified as Cards (acrossSpaceAndTime)
import Arkham.Treachery.Import.Lifted

newtype AcrossSpaceAndTime = AcrossSpaceAndTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acrossSpaceAndTime :: TreacheryCard AcrossSpaceAndTime
acrossSpaceAndTime = treachery AcrossSpaceAndTime Cards.acrossSpaceAndTime

instance RunMessage AcrossSpaceAndTime where
  runMessage msg t@(AcrossSpaceAndTime attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      discardTopOfDeck iid attrs 3
      pure t
    _ -> AcrossSpaceAndTime <$> liftRunMessage msg attrs
