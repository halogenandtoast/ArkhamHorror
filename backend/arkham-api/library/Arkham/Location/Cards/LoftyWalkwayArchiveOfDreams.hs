module Arkham.Location.Cards.LoftyWalkwayArchiveOfDreams (loftyWalkwayArchiveOfDreams) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LoftyWalkwayArchiveOfDreams = LoftyWalkwayArchiveOfDreams LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

loftyWalkwayArchiveOfDreams :: LocationCard LoftyWalkwayArchiveOfDreams
loftyWalkwayArchiveOfDreams = location LoftyWalkwayArchiveOfDreams Cards.loftyWalkwayArchiveOfDreams 5 (Static 1)

-- TODO: abilities

instance RunMessage LoftyWalkwayArchiveOfDreams where
  runMessage msg (LoftyWalkwayArchiveOfDreams attrs) = runQueueT $ LoftyWalkwayArchiveOfDreams <$> liftRunMessage msg attrs
