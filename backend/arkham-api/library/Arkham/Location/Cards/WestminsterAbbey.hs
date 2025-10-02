module Arkham.Location.Cards.WestminsterAbbey (westminsterAbbey) where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype WestminsterAbbey = WestminsterAbbey LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

westminsterAbbey :: LocationCard WestminsterAbbey
westminsterAbbey = symbolLabel $ location WestminsterAbbey Cards.westminsterAbbey 1 (PerPlayer 1)

instance HasAbilities WestminsterAbbey where
  getAbilities (WestminsterAbbey attrs) =
    extendRevealed attrs []

instance RunMessage WestminsterAbbey where
  runMessage msg (WestminsterAbbey attrs) = runQueueT $ case msg of
    _ -> WestminsterAbbey <$> liftRunMessage msg attrs
