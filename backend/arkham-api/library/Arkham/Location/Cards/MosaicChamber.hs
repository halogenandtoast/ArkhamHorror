module Arkham.Location.Cards.MosaicChamber (mosaicChamber) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype MosaicChamber = MosaicChamber LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mosaicChamber :: LocationCard MosaicChamber
mosaicChamber = location MosaicChamber Cards.mosaicChamber 3 (PerPlayer 1)

instance HasAbilities MosaicChamber where
  getAbilities (MosaicChamber attrs) =
    extendRevealed attrs []

instance RunMessage MosaicChamber where
  runMessage msg (MosaicChamber attrs) = runQueueT $ case msg of
    _ -> MosaicChamber <$> liftRunMessage msg attrs
