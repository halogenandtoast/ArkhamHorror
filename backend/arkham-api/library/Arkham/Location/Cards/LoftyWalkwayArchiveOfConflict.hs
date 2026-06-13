module Arkham.Location.Cards.LoftyWalkwayArchiveOfConflict (loftyWalkwayArchiveOfConflict) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LoftyWalkwayArchiveOfConflict = LoftyWalkwayArchiveOfConflict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loftyWalkwayArchiveOfConflict :: LocationCard LoftyWalkwayArchiveOfConflict
loftyWalkwayArchiveOfConflict = location LoftyWalkwayArchiveOfConflict Cards.loftyWalkwayArchiveOfConflict 2 (Static 1)

-- TODO: abilities

instance RunMessage LoftyWalkwayArchiveOfConflict where
  runMessage msg (LoftyWalkwayArchiveOfConflict attrs) = runQueueT $ LoftyWalkwayArchiveOfConflict <$> liftRunMessage msg attrs
