module Arkham.Location.Cards.RingLibraryArchiveOfTheAncients (ringLibraryArchiveOfTheAncients) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype RingLibraryArchiveOfTheAncients = RingLibraryArchiveOfTheAncients LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ringLibraryArchiveOfTheAncients :: LocationCard RingLibraryArchiveOfTheAncients
ringLibraryArchiveOfTheAncients = location RingLibraryArchiveOfTheAncients Cards.ringLibraryArchiveOfTheAncients 3 (Static 1)

-- TODO: abilities

instance RunMessage RingLibraryArchiveOfTheAncients where
  runMessage msg (RingLibraryArchiveOfTheAncients attrs) = runQueueT $ RingLibraryArchiveOfTheAncients <$> liftRunMessage msg attrs
