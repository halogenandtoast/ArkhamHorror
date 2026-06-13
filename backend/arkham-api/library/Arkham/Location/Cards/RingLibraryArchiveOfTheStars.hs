module Arkham.Location.Cards.RingLibraryArchiveOfTheStars (ringLibraryArchiveOfTheStars) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RingLibraryArchiveOfTheStars = RingLibraryArchiveOfTheStars LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ringLibraryArchiveOfTheStars :: LocationCard RingLibraryArchiveOfTheStars
ringLibraryArchiveOfTheStars = location RingLibraryArchiveOfTheStars Cards.ringLibraryArchiveOfTheStars 2 (Static 2)

-- TODO: abilities

instance RunMessage RingLibraryArchiveOfTheStars where
  runMessage msg (RingLibraryArchiveOfTheStars attrs) = runQueueT $ RingLibraryArchiveOfTheStars <$> liftRunMessage msg attrs
